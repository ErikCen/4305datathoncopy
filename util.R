# Sets the types for variables common to all 3 books
set_types = function(df) {
  df %>%
    mutate(
      VehValue         = as.numeric(VehValue),
      VehicleType      = as.factor(VehicleType),
      RiskZone         = as.factor(RiskZone),
      AnnualKms        = factor(AnnualKms, levels = c("Low", "Medium", "High")),
      GaragingLocation = as.factor(GaragingLocation),
      VehicleUse       = as.factor(VehicleUse),
      PaymentFrequency = as.factor(PaymentFrequency),
      MultiPolicyFlag  = as.logical(MultiPolicyFlag),
    )
}