# Insurance Ratios

suppressMessages(library("tidyverse"))

price <- function(cost, profit) {
  price <- cost + profit
  return(price)
}

reported_loss <- function(paid_loss, current_case_reserve) {
  reported_loss <- paid_loss + current_case_reserve
  return(reported_loss)
}

ultimate_loss <- function (reported_loss, IBNR_reserve, IBNER_reserve) {
  ultimate_loss <- reported_loss + IBNR_reserve + IBNER_reserve
  return(ultimate_loss)
}

LAE <- function (ALAE, ULAE) {
  LAE <- ALAE + ULAE
  return(LAE)
}

fundamental_insurance_equation <-
  function(loss, LAE, UW_expenses, UW_profit) {
    premium <- loss + LAE + UW_expenses + UW_profit
    return(premium)
  }

frequency <- function (n_claims, n_exposures) {
  frequency <- n_claims/n_exposures
  return(frequency)
}

severity <- function (losses, n_claims) {
  severity <- losses/n_claims
  return(severity)
}

pure_premium <- function (frequency, severity) {
  # AKA loss cost, burning cost
  # losses/n_exposures
  pure_premium <- frequency*severity
  return(pure_premium)
}

average_premium <- function (premium, n_exposures) {
  average_premium <- premium/n_exposures
  return(average_premium)
}

loss_ratio <- function (pure_premium, average_premium) {
  # loss_ratio <- losses/premium
  loss_ratio <- pure_premium/average_premium
  return(loss_ratio)
}

LAE_ratio <- function (LAE, losses) {
  LAE_ratio <- LAE/losses
  return(LAE_ratio)
}

UW_expense_ratio <- function (UW_expenses, premium) {
  UW_expense_ratio <- UW_expenses/premium
  return(UW_expense_ratio)
}

operating_expense_ratio <- function (UW_expense_ratio, LAE, earned_premium) {
  operating_expense_ratio <- UW_expense_ratio + LAE/earned_premium
  return(operating_expense_ratio)
}

combined_ratio <- function (loss_ratio,
                            LAE,
                            earned_premium,
                            UW_expenses,
                            written_premium) {
  combined_ratio <-
    loss_ratio +
    LAE/earned_premium +
    UW_expenses/written_premium
  # combined_ratio <- loss_ratio + OER
  return(combined_ratio)
}

retention_ratio <- function () {
  retention_ratio <- n_policies_renewed/n_potential_renewal_policies
  return(retention_ratio)
}

close_ratio <- function (n_accepted_quotes, n_quotes) {
  close_ratio <- n_accepted_quotes/n_quotes
  return(close_ratio)
}

# Pure Premium Method
indicated_average_rate <- function (pure_premium_with_lae,
                                    fixed_UW_expense_per_exposure,
                                    variable_expense_percent,
                                    target_UW_profit_percent) {
  indicated_average_rate <- 
    (pure_premium_with_lae + fixed_UW_expense_per_exposure)/
    (1 - variable_expense_percent - target_UW_profit_percent)
  return(indicated_average_rate)
}

# Loss Ratio Method
indicated_change_factor <- function (loss_LAE_ratio,
                                     fixed_expense_ratio,
                                     variable_expense_percent,
                                     target_UW_profit_percent) {
  indicated_change_factor <-
    (loss_LAE_ratio + fixed_expense_ratio)/
    (1 - variable_expense_percent - target_UW_profit_percent)
  return(indicated_change_factor)
}
