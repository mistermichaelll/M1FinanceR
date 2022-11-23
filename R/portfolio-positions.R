#' Get the current positions in your portfolio.
#'
#' @param account_number a user's M1 Finance account number.
#'
#' @details
#' This function returns top-level information about a user's portfolio, including the market value
#' and the number of shares they own.
#'
get_portfolio_positions <- function(account_number) {
  url <- sprintf(
    "https://api.apexclearing.com/margin-provider/api/v1/positions/%s",
    account_number
  )

  response <-
    suppressMessages(
      GET(
        url,
        add_headers(
          Cookie = Sys.getenv("APEX_token"),
          ContentType = "application/json;charset=UTF-8"
        )
      )
    )

  if (response$status_code != 200) {
    stop(sprintf("ERROR: portfolio pull was unsuccessful, status code %s. Are you sure your account number is correct?", response$status_code))
  }

  response_json <-
    response |>
    content(as = "text", encoding = "UTF-8") |>
    parse_json()

  portfolio_positions <-
    response_json |>
    pluck(1, "positions") |>
    map_df(flatten_df)

  return(portfolio_positions)
}
