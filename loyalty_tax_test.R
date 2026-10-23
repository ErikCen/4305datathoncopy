#Task 2B

library(dplyr)

# 1. Restore original policy characteristics from renewal file
renewal27_original <- read.csv(
  "C:/Users/zhcen/Downloads/renewal_book_2027.csv"
)

# Restore tenure and other variables that may have been overwritten
restore_cols <- c(
  "PolicyID", "TenureYears", "NCDLevel",
  "MultiPolicyFlag", "PaymentFrequency"
)

technical27_commercial <- technical27_commercial %>%
  select(-any_of(c("TenureYears_original")))

technical27_commercial <- technical27_commercial %>%
  select(-any_of(setdiff(restore_cols, "PolicyID"))) %>%
  left_join(
    renewal27_original %>% select(all_of(restore_cols)),
    by = "PolicyID"
  )

# Standardise multi-policy flag
technical27_commercial$MultiPolicyFlag <- factor(
  ifelse(
    as.character(technical27_commercial$MultiPolicyFlag) %in%
      c("1", "TRUE"),
    "TRUE", "FALSE"
  )
)

# 2. Rebuild Task 2B data
loyalty_data <- technical27_commercial %>%
  filter(
    is.finite(TechnicalPrice),
    TechnicalPrice > 0,
    is.finite(FinalPremium),
    FinalPremium > 0,
    !is.na(TenureYears)
  ) %>%
  mutate(
    RelativePremium = FinalPremium / TechnicalPrice,
    TenureBand = case_when(
      TenureYears <= 2 ~ "0-2 years",
      TenureYears <= 5 ~ "3-5 years",
      TenureYears <= 9 ~ "6-9 years",
      TenureYears >= 10 ~ "10+ years"
    ),
    TenureBand = factor(
      TenureBand,
      levels = c("0-2 years", "3-5 years", "6-9 years", "10+ years")
    )
  ) %>%
  filter(!is.na(TenureBand)) %>%
  droplevels()

# Check tenure groups
print(table(loyalty_data$TenureBand))

# 3. Summary by tenure group
tenure_summary <- loyalty_data %>%
  group_by(TenureBand) %>%
  summarise(
    Policies = n(),
    MeanTechnicalPrice = mean(TechnicalPrice),
    MeanFinalPremium = mean(FinalPremium),
    MeanRelativePremium = mean(RelativePremium),
    MedianRelativePremium = median(RelativePremium),
    .groups = "drop"
  )

print(tenure_summary)

# 4. Keep only predictors with variation
categorical_vars <- c(
  "VehicleType", "RiskZone", "AnnualKms",
  "GaragingLocation", "VehicleUse",
  "MultiPolicyFlag", "PaymentFrequency"
)

categorical_vars <- categorical_vars[
  sapply(technical27_commercial[categorical_vars], function(x) {
    nlevels(droplevels(as.factor(x))) >= 2
  })
]

numeric_vars <- c(
  "DriverAge", "YearsLicensed", "VehAge",
  "VehValue", "Density", "ExcessChosen", "NCDLevel"
)

numeric_vars <- numeric_vars[
  sapply(loyalty_data[numeric_vars], function(x) {
    length(unique(na.omit(x))) >= 2
  })
]

# 5. Risk-adjusted regression
model_formula <- as.formula(
  paste(
    "log(RelativePremium) ~ TenureBand +",
    paste(c(categorical_vars, numeric_vars), collapse = " + ")
  )
)

loyalty_model <- lm(
  model_formula,
  data = loyalty_data,
  na.action = na.exclude
)

print(summary(loyalty_model))

# 6. Adjusted tenure differences
model_results <- as.data.frame(coef(summary(loyalty_model)))
model_results$Term <- rownames(model_results)
rownames(model_results) <- NULL

tenure_results <- model_results %>%
  filter(grepl("^TenureBand", Term)) %>%
  mutate(
    AdjustedDifferencePct = (exp(Estimate) - 1) * 100,
    SignificantAt5Pct = `Pr(>|t|)` < 0.05
  )

print(tenure_results)

# 7. Save outputs
write.csv(
  tenure_summary,
  "C:/Users/zhcen/Downloads/task2b_tenure_summary.csv",
  row.names = FALSE
)

write.csv(
  tenure_results,
  "C:/Users/zhcen/Downloads/task2b_risk_adjusted_results.csv",
  row.names = FALSE
)

cat("Task 2B completed.\n")