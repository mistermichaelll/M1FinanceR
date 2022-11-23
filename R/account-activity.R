#' Get the activity in an account from a given start and end date, including buys, sells, and other money movements like dividends.
#'
#' @param account_number a user's M1 Finance account number.
#' @param start_date a string containing the starting date for the activity export, must not be greater than 2 years before the end date.
#' @param end_date a string containing the end date for the activity export.
#'
get_account_activity <- function(account_number, start_date, end_date) {
  url <- paste0(
    "https://api.apexclearing.com/activities-provider/api/v1/activities/",
    account_number,
    "?activityType=TRADES&activityType=MONEY_MOVEMENTS&activityType=POSITION_ADJUSTMENTS&",
    "endDate=", end_date,
    "&startDate=", start_date
  )

  response <-
    GET(
      url,
      add_headers(
        Cookie = Sys.getenv("APEX_token")
      )
    )

  response_json <-
    response |>
    content(as = "text", encoding = "UTF-8") |>
    parse_json()

  suppressMessages(
      response_json |>
        map(
          ~pluck(.x)
        ) |>
        map_df(flatten_dfc)
  )
}
