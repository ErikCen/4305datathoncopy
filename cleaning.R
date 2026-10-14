library(dplyr)
library(stringr)
library(stringdist)
library(janitor)
library(readr)

# ---------------------------------------------------------------------------
# Helper: fuzzy-match a character vector to a set of allowed levels
# ---------------------------------------------------------------------------
.match_levels <- function(x, allowed, max_dist, label, verbose) {
  x_chr <- as.character(x)
  x_chr[trimws(x_chr) == ""] <- NA            # blank strings are missing, not "close" to a level

  idx <- stringdist::amatch(
    stringr::str_to_lower(trimws(x_chr)),
    stringr::str_to_lower(allowed),
    maxDist = max_dist
  )
  out <- allowed[idx]

  n_unmatched <- sum(!is.na(x_chr) & is.na(out))
  if (verbose && n_unmatched > 0) {
    message(sprintf("  %s: %d value(s) could not be matched and were set to NA", label, n_unmatched))
  }

  factor(out, levels = allowed)
}

# ---------------------------------------------------------------------------
# Main cleaning function
#
# raw         : a data.frame, or a path to a raw claims CSV
# output_path : optional path; if given, the cleaned data is written there
# max_dist    : maximum string distance allowed when matching category labels
# max_age     : drivers at or above this age are treated as data errors
# verbose     : print a row-count log of each step
#
# Returns the cleaned data frame (invisibly if written to disk).
# ---------------------------------------------------------------------------
clean_claims <- function(data,
                         max_dist    = 3,
                         max_age     = 120,
                         verbose     = TRUE) {

  # Allowed category levels
  vehicle_types      <- c("Sedan", "SUV", "Hatch", "Ute", "Sports")
  garaging_locations <- c("Garage", "Street", "Carport")
  vehicle_uses       <- c("Private", "Private_Business")

  log_step <- function(label, df) {
    if (verbose) message(sprintf("%-45s %d rows", label, nrow(df)))
  }

  required <- c("VehValue", "RiskZone", "AnnualKms", "PaymentFrequency",
                "MultiPolicyFlag", "Exposure", "VehicleType", "GaragingLocation",
                "VehicleUse", "ExcessChosen", "DriverAge", "VehAge", "ClaimAmount")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Raw data is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
  log_step("Loaded raw data", data)

  # 1. Fix column types -----------------------------------------------------
  data <- data |>
    mutate(
      VehValue         = as.numeric(VehValue),
      RiskZone         = as.factor(RiskZone),
      AnnualKms        = as.factor(AnnualKms),
      PaymentFrequency = as.factor(PaymentFrequency),
      MultiPolicyFlag  = as.logical(MultiPolicyFlag),
      Exposure         = as.numeric(Exposure)
    )

  # 2. Standardise category labels (e.g. "suv" / "Suv" -> "SUV") -------------
  data <- data |>
    mutate(
      VehicleType      = .match_levels(VehicleType,      vehicle_types,      max_dist, "VehicleType",      verbose),
      GaragingLocation = .match_levels(GaragingLocation, garaging_locations, max_dist, "GaragingLocation", verbose),
      VehicleUse       = .match_levels(VehicleUse,       vehicle_uses,       max_dist, "VehicleUse",       verbose)
    )

  # 3. Fix invalid numeric values -------------------------------------------
  #    - ExcessChosen: 999 is a legacy 3-digit-field code for 1000
  #    - DriverAge:    -1 is a missing-value code; ages >= max_age are errors (e.g. 150)
  #    - VehAge:       negative values are impossible
  data <- data |>
    mutate(
      ExcessChosen = as.integer(replace(ExcessChosen,
                                        !is.na(ExcessChosen) & ExcessChosen == 999, 1000L)),
      DriverAge    = replace(DriverAge, !is.na(DriverAge) & (DriverAge == -1 | DriverAge < max_age), NA),
      VehAge       = replace(VehAge,    !is.na(VehAge) & VehAge < 0, NA)
    )

  # 4. Recover missing values within PolicyID -------------------------------
  #    Uses longitudinal structure (PolicyID + Year) to fill NAs in
  #    DriverAge, VehValue, AnnualKms and GaragingLocation.
  if (all(c("PolicyID", "Year") %in% names(data))) {
    
    # -- helpers ------------------------------------------------------------
    fill_factor <- function(x) {
      vals <- x[!is.na(x)]
      if (length(vals) == 0) return(x)
      if (length(unique(vals)) == 1) {
        val <- vals[1]
        if (is.factor(x)) val <- as.character(val)
        x[is.na(x)] <- val
      }
      x
    }
    
    fill_age <- function(age, year) {
      miss <- is.na(age)
      if (!any(miss)) return(age)
      known <- which(!is.na(age) & !is.na(year))
      if (length(known) == 0) return(age)
      for (i in which(miss)) {
        if (is.na(year[i])) next
        prev <- known[year[known] < year[i]]
        if (length(prev) > 0) {
          ref <- prev[which.max(year[prev])]
        } else {
          ref <- known[which.min(year[known])]
        }
        age[i] <- age[ref] + (year[i] - year[ref])
      }
      age
    }
    
    fill_veh <- function(value, year) {
      ok <- !is.na(value) & !is.na(year)
      if (sum(ok) < 2) return(value)
      fit  <- lm(value ~ year, data = data.frame(value = value[ok], year = year[ok]))
      pred <- predict(fit, newdata = data.frame(year = year))
      value[is.na(value)] <- pmax(0, pred[is.na(value)])
      value
    }
    
    # -- apply --------------------------------------------------------------
    if (verbose) {
      before <- sapply(data[c("DriverAge","VehValue","AnnualKms","GaragingLocation")],
                       function(x) sum(is.na(x)))
      message("  Missing before recovery: ",
              paste(names(before), before, sep = "=", collapse = ", "))
    }
    
    data <- data |>
      mutate(Year = as.numeric(Year)) |>
      arrange(PolicyID, Year) |>
      group_by(PolicyID) |>
      mutate(
        AnnualKms        = fill_factor(AnnualKms),
        GaragingLocation = fill_factor(GaragingLocation),
        DriverAge        = fill_age(DriverAge, Year),
        VehValue         = fill_veh(VehValue, Year)
      ) |>
      ungroup()
    
    if (verbose) {
      after <- sapply(data[c("DriverAge","VehValue","AnnualKms","GaragingLocation")],
                      function(x) sum(is.na(x)))
      message("  Missing after recovery:  ",
              paste(names(after), after, sep = "=", collapse = ", "))
    }
    log_step("After within-policy recovery", data)
  } else if (verbose) {
    message("  Skipping within-policy recovery (PolicyID/Year not present)")
  }
  # 4. Drop rows with any missing values ------------------------------------
  if (verbose) {
    n_miss <- sum(!complete.cases(data))
    message(sprintf("  Rows with missing values: %d (%.2f%%)", n_miss, 100 * n_miss / nrow(data)))
  }
  data <- na.omit(data)
  log_step("After removing rows with NAs", data)

  # 5. Drop duplicate rows --------------------------------------------------
  if (verbose) {
    message(sprintf("  Duplicate rows: %d", nrow(data) - nrow(unique(data))))
  }
  data <- unique(data)
  log_step("After removing duplicates", data)

  data
}

# raw = read.csv("data/claims_history_2023_2026.csv", header = T)
# cleaned <- clean_claims(raw)
# write.csv(cleaned, "data/book26.csv")
