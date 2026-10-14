# File contains all the discount pricing formulas based on their parameters
library(dplyr)

get_NoClaimDiscount = function(NCDLevel) {
  0.1 * NCDLevel
}

get_LoyaltyDiscount = function(TenureYears) {
  case_when(
    between(TenureYears, 0, 2) ~ 0.00,
    between(TenureYears, 3, 5) ~ 0.03,
    between(TenureYears, 6, 9) ~ 0.06,
    TenureYears >= 10          ~ 0.10
  )
}

get_MultiPolicyDiscount = function(MultiPolicyFlag) {
  if_else(MultiPolicyFlag == 1, 0.1, 0)
}