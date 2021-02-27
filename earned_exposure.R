# Earned Exposure

suppressMessages(library("tidyverse"))
suppressMessages(library("lubridate"))

# Calendar Year Earned Exposure Calculation

# Inputs:
# Policy transaction record
# Transaction start date,
# transaction end date,
# year of interest,
# policy term in years
# Output:
# Earned exposure

# Calendar Year Earned Days Calculation

calendar_year_earned_days_calculation <-
  function (start_date, end_date, year_of_interest) {
    
    first_day_of_year_of_interest <- ymd(str_c(year_of_interest,
                                               "-01-01"))
    first_day_of_next_year_of_interest <- ymd(str_c(year_of_interest + 1,
                                                    "-01-01"))
    if (start_date > end_date) {
      earned_days <- NA_real_
    } else if ((start_date < first_day_of_year_of_interest &
                end_date <= first_day_of_year_of_interest) |
               (start_date >= first_day_of_next_year_of_interest &
                end_date >= first_day_of_next_year_of_interest)) {
      earned_days <- 0
    } else if (start_date <= first_day_of_year_of_interest &
               end_date >= first_day_of_next_year_of_interest) {
      earned_days <-
        first_day_of_next_year_of_interest - first_day_of_year_of_interest
    } else if (start_date >= first_day_of_year_of_interest &
               end_date <= first_day_of_next_year_of_interest) {
      earned_days <- end_date - start_date
    } else if (star_date >= first_day_of_year_of_interest &
               end_date > first_day_of_next_year_of_interest) {
      earned_days <- first_day_of_next_year_of_interest - start_date
    } else if (start_date < first_day_of_year_of_interest &
               end_date < first_day_of_next_year_of_interest) {
      earned_days <- first_day_of_next_year_of_interest - end_date
    } else {
      stop("Dates don't make sense or function needs improvement")
    }
    return(earned_days)
  }

# Earned Exposure
calendar_year_earned_exposure_calculation <-
  function (policy_term) {
  }
