#' Helper function to clean up code which generates realized gains and losses.
#'
#' @param json_response the API response in JSON format.
#' @param short_or_long one of either "short" or "long" for the term in which gains are evaluated.
#'
get_gain_loss_by_term <- function(
    json_response,
    short_or_long = c("short", "long")
) {
  short_or_long <- switch(
    short_or_long,
    "short" = "stGainLoss",
    "long" = "ltGainLoss"
  )

  json_response |>
    pluck("lot") |>
    map(
      ~pluck(.x, short_or_long)
    ) |>
    unlist()
}

#' Get realized gains and losses for a given portfolio.
#'
#' @param account_number a user's M1 Finance account number.
#'
get_realized_gains_losses <- function(account_number) {
  url <-
    sprintf(
      "https://api.apexclearing.com/taxman/api/v1/gainloss/%s/realized?fromdate=19000101&page=1&pagesize=10000",
      account_number
    )

  response <-
    GET(
      url,
      add_headers(
        Cookie = Sys.getenv("APEX_token")
      )
    )

  if (response$status_code != 200) {
    stop(sprintf("ERROR: API connection was unsuccessful, status code %s. Are you sure your account number is correct?", response$status_code))
  }

  response_json <-
    response |>
    content(as = "text", encoding = "UTF-8") |>
    parse_json()

  securities <-
    response_json |>
    pluck("lot") |>
    map(
      ~pluck(.x, "security")
    ) |>
    map_dfr(flatten_df)

  long_short <-
    response_json |>
    pluck("lot") |>
    map(
      ~pluck(.x, "longShortInd")
    ) |>
    unlist()

  buys <-
    response_json |>
    pluck("lot") |>
    map(
      ~pluck(.x, "buy")
    ) |>
    map_dfr(flatten_df) |>
    mutate(
      securities,
      long_short,
      buy_sell = "BUY"
    )

  sells <-
    response_json |>
    pluck("lot") |>
    map(
      ~pluck(.x, "sell")
    ) |>
    map_dfr(flatten_df) |>
    mutate(
      securities,
      long_short,
      long_term_gain_loss = get_gain_loss_by_term(json_response = response_json, "long"),
      short_term_gain_loss = get_gain_loss_by_term(json_response = response_json, "short"),
      buy_sell = "SELL"
    )

  bind_rows(buys, sells) |>
    relocate(.data$symbol, .data$buy_sell)
}

#' Get the open positions in your portfolio.
#'
#' @param account_number a user's M1 Finance account number.
#'
get_open_positions <- function(account_number) {
  url <-
    sprintf(
      "https://api.apexclearing.com/taxman/api/v1/gainloss/%s/unrealized?fromdate=19000101&page=1&pagesize=10000",
      account_number
    )

  response <-
    GET(
      url,
      add_headers(
        Cookie = Sys.getenv("APEX_token")
      )
    )

  if (response$status_code != 200) {
    stop(sprintf("ERROR: API connection was unsuccessful, status code %s. Are you sure your account number is correct?", response$status_code))
  }

  response_json <-
    response |>
    content(as = "text", encoding = "UTF-8") |>
    parse_json()

  securities <-
    response_json |>
    pluck("lot") |>
    map(
      ~pluck(.x, "security")
    ) |>
    map_dfr(flatten_df)

  long_short <-
    response_json |>
    pluck("lot") |>
    map(
      ~pluck(.x, "longShortInd")
    ) |>
    unlist()

  response_json |>
    pluck("lot") |>
    map(
      ~pluck(.x, "buy")
    ) |>
    map_dfr(flatten_df) |>
    mutate(
      securities,
      long_short,
      buy_sell = "BUY"
    ) |>
    relocate(.data$symbol, .data$buy_sell)
}
