library(MuMIn)
library(dplyr)

# 1. Prepare all data in the GLOBAL environment
book26 <- read.csv("data/book26.csv")

severity_data <- book26 %>%
  filter(ClaimNb > 0, ClaimAmount > 0) %>%
  mutate(AvgSeverity = ClaimAmount / ClaimNb)

# 2. Define predictors and formulas
predictors <- c(
  "DriverAge", "YearsLicensed", "VehAge",
  "VehValue", "VehicleType", "RiskZone",
  "Density", "AnnualKms", "ExcessChosen",
  "GaragingLocation", "VehicleUse",
  "NCDLevel", "TenureYears",
  "MultiPolicyFlag", "PaymentFrequency"
)

rhs <- paste(predictors, collapse = " + ")
freq_formula <- as.formula(paste("ClaimNb ~", rhs))
sev_formula <- as.formula(paste("AvgSeverity ~", rhs))

# 3. Fit Global Models
global_freq <- glm(
  freq_formula, 
  family = poisson(link = "log"), 
  data = book26,
  na.action = "na.fail"
)

global_sev <- glm(
  sev_formula,
  family = Gamma(link = "log"),
  weights = ClaimNb,
  data = severity_data,
  na.action = "na.fail"
)

# 4. Dredge the models
# NOTE: m.lim = c(0, 5) limits models to a maximum of 5 predictors. 
# Remove it only if you are prepared to wait for 32,768 models to compile!

# AICc models
all_freq_models_aic <- dredge(global_freq, rank = "AICc", m.lim = c(0, 5))
all_sev_models_aic  <- dredge(global_sev, rank = "AICc", m.lim = c(0, 5))

# BIC models (Fixed variable names)
all_freq_models_bic <- dredge(global_freq, rank = "BIC", m.lim = c(0, 5))
all_sev_models_bic  <- dredge(global_sev, rank = "BIC", m.lim = c(0, 5))