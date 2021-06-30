# Earned Exposure

suppressMessages(library("tidyverse"))
suppressMessages(library("lubridate"))

# Calendar Year Earned Exposure Calculation

# Inputs (policy transaction record):
# Transaction start date,
# transaction end date,
# year of interest,
# policy term length in years
# Output:
# Earned exposure

# Calendar Year Earned Exposure Calculation
calendar_year_earned_exposure_calculation <-
  function (start_date, end_date, year_of_interest) {
    first_day_of_year_of_interest <- ymd(str_c(year_of_interest,
                                               "-01-01"))
    first_day_of_next_year_of_interest <- ymd(str_c(year_of_interest + 1,
                                                    "-01-01"))
    days_in_year_of_interest <-
      first_day_of_next_year_of_interest - first_day_of_year_of_interest
    days_in_year_of_interest <- as.integer(days_in_year_of_interest)
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
    } else if (start_date >= first_day_of_year_of_interest &
               end_date > first_day_of_next_year_of_interest) {
      earned_days <- first_day_of_next_year_of_interest - start_date
      
    } else if (start_date < first_day_of_year_of_interest &
               end_date < first_day_of_next_year_of_interest) {
      earned_days <- end_date - first_day_of_year_of_interest
    } else {
      stop("Dates don't make sense or function needs improvement")
    }
    earned_days <- as.integer(earned_days)
    earned_exposure <- earned_days/days_in_year_of_interest
    return(earned_exposure)
  }

### TESTS

calendar_year_earned_exposure_calculation(start_date=ymd("2014-03-17"),
                                          end_date=ymd("2015-03-17"),
                                          year_of_interest=2015L)
# 31 + 28 + 16 days (75 days) / 365 days (0.2054795)

calendar_year_earned_exposure_calculation(start_date=ymd("2015-03-17"),
                                          end_date=ymd("2015-03-17"),
                                          year_of_interest=2015L)
# 0 days / 365 days (0)

calendar_year_earned_exposure_calculation(start_date=ymd("2014-03-17"),
                                          end_date=ymd("2016-03-17"),
                                          year_of_interest=2015L)
# 365 days / 365 days (1)

calendar_year_earned_exposure_calculation(start_date=ymd("2014-03-17"),
                                          end_date=ymd("2014-09-17"),
                                          year_of_interest=2015L)
# 0 days / 365 days (0)

calendar_year_earned_exposure_calculation(start_date=ymd("2015-03-17"),
                                          end_date=ymd("2015-09-17"),
                                          year_of_interest=2015L)
# (31 - 17 + 1) + 30 + 31 + 30 + 31 + 31 + 16 (184 days) / 365 days (0.5041096)

calendar_year_earned_exposure_calculation(start_date=ymd("2015-03-17"),
                                          end_date=ymd("2016-03-17"),
                                          year_of_interest=2015L)
# (31 - 17 + 1) + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31 (290 days)
# / 365 days (0.7945205)

### TESTS

# Inputs:
# policy term length in years

# policy_term_length < 1
# policy_term_length > 1
# 0 < earned_exposure < 1

# % of policy term earned
