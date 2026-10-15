source("discount.R")
waterfall = function(premium, policy, steps) {
  for (step in steps) premium = step(premium, policy)
  premium
}

bounds_step = function(collar, cap) function(p, pol) {
  ifelse(is.na(pol$Baseline), 
    p,
    pmin(pmax(p, pol$Baseline * (1 - collar)), pol$Baseline * (1 + cap)))
}

discount_step = function(operation) function(p, pol) {
  with(pol, {
    if (operation == "add" | operation == "addition") {
      discount_factor = 1 - (get_NoClaimDiscount(NCDLevel) + 
                             get_LoyaltyDiscount(TenureYears) + 
                             get_MultiPolicyDiscount(MultiPolicyFlag))
      
    } else if (operation == "multiply" | operation == "multiplication" | operation == "times") { # multiply operation
      discount_factor = (1 - get_NoClaimDiscount(NCDLevel)) *
                        (1 - get_LoyaltyDiscount(TenureYears)) *
                        (1 - get_MultiPolicyDiscount(MultiPolicyFlag))
    } else {
      discount_factor = NA
    }
    p * discount_factor
  })
}

pe_loading_step = function(fixed_cost = 85, variable_expense_rate = 0.1, profit_margin = 0.08) function(p, pol) {
  (p + fixed_cost) / (1 - variable_expense_rate - profit_margin)
}

instalment_loading_step = function(instalment_loading = 0.03) function(p, pol) {
  ifelse (pol$PaymentFrequency == "Monthly", p * (1 + instalment_loading), p)
}

