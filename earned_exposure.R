############################### EARNED EXPOSURE ################################

suppressMessages(library("tidyverse"))
suppressMessages(library("lubridate"))

# Calendar Period Earned Exposure
calendar_period_earned_exposure <-
  function (transaction_start_date,
            transaction_end_date,
            calendar_period_start_date,
            calendar_period_end_date) {
    days_in_calendar_period <-
      calendar_period_end_date - calendar_period_start_date
    days_in_calendar_period <- days_in_calendar_period |>
      as.integer()
    if (transaction_start_date > transaction_end_date) {
      earned_days <- NA_real_
    } else if ((transaction_start_date < calendar_period_start_date &
                transaction_end_date <= calendar_period_start_date) |
               (transaction_start_date >= calendar_period_end_date &
                transaction_end_date >= calendar_period_end_date)) {
      earned_days <- 0
    } else if (transaction_start_date <= calendar_period_start_date &
               transaction_end_date >= calendar_period_end_date) {
      earned_days <-
        calendar_period_end_date -
        calendar_period_start_date
    } else if (transaction_start_date >= calendar_period_start_date &
               transaction_end_date <= calendar_period_end_date) {
      earned_days <-
        transaction_end_date -
        transaction_start_date
    } else if (transaction_start_date >= calendar_period_start_date &
               transaction_end_date > calendar_period_end_date) {
      earned_days <-
        calendar_period_end_date -
        transaction_start_date
      
    } else if (transaction_start_date < calendar_period_start_date &
               transaction_end_date < calendar_period_end_date) {
      earned_days <-
        transaction_end_date -
        calendar_period_start_date
    } else {
      stop("Dates don't make sense or function needs improvement")
    }
    earned_days <- as.integer(earned_days)
    earned_exposure <- earned_days/days_in_calendar_period
    return(earned_exposure)
  }

# Example
calendar_period_earned_exposure(
  transaction_start_date=ymd("2015-03-17"),
  transaction_end_date=ymd("2016-03-17"),
  calendar_period_start_date=ymd("2015-01-01"),
  calendar_period_end_date=ymd("2016-01-01")
)

# Earned Exposure Calculation
earned_exposure_calculation <-
  function (transactions,
            calendar_period_start_date,
            calendar_period_length_in_years,
            number_of_calendar_periods) {
    calendar_periods <-
      calendar_period_start_date +
      months(as.integer(calendar_period_length_in_years*12))*
      seq(0L, number_of_calendar_periods, 1L)
    calendar_periods <- tibble(
      CalendarPeriodStartDate=
        head(calendar_periods, length(calendar_periods) - 1),
      CalendarPeriodEndDate=
        tail(calendar_periods, length(calendar_periods) - 1)
    )
    transactions <- transactions %>%
      expand_grid(calendar_periods) %>%
      mutate(
        EarnedExposure=
          mapply(
            FUN=calendar_period_earned_exposure,
            transaction_start_date=TransactionStartDate,
            transaction_end_date=TransactionEndDate,
            calendar_period_start_date=CalendarPeriodStartDate,
            calendar_period_end_date=CalendarPeriodEndDate
          )
      )
    return(transactions)
  }

######################## SIMULATED TRANSACTIONS EXAMPLE ########################

set.seed(394)
unique_ids <- seq(from=1L, to=1000L, by=1L) |> as.character()
transaction_start_dates <- sample(x=seq(from=ymd("2015-01-01"),
                                        to=ymd("2015-12-31"),
                                        by=1L),
                                  size=1000L,
                                  replace=TRUE)
transaction_end_dates <-
  transaction_start_dates +
  days(sample(x=seq(from=0L, to=366L, by=1L), size=1000L, replace=TRUE))
transactions <- tibble(
  UniqueID=unique_ids,
  TransactionStartDate=transaction_start_dates,
  TransactionEndDate=transaction_end_dates
)
rm(unique_ids,
   transaction_start_dates,
   transaction_end_dates)
transactions <- earned_exposure_calculation(
  transactions=transactions,
  calendar_period_start_date=ymd("2010-01-01"),
  calendar_period_length_in_years=1L,
  number_of_calendar_periods=10L
)
head(transactions, 20)

######################## SIMULATED TRANSACTIONS EXAMPLE ########################

############################### EARNED EXPOSURE ################################
