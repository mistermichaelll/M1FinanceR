#' Get realized gains and losses for a given portfolio.
#'
#' @param account_number a user's M1 Finance account number.
#'
get_realized_gains_losses <- function(account_number){
    url <-
        sprintf(
            "https://api.apexclearing.com/taxman/api/v1/gainloss/%s/realized?fromdate=19000101&page=1&pagesize=10000",
            account_number
        )

    response <-
        GET(url,
            add_headers(
                Cookie = Sys.getenv("APEX_token")
            )
        )

    if(response$status_code != 200){
        stop(sprintf("ERROR: API connection was unsuccessful, status code %s. Are you sure your account number is correct?", response$status_code))
    }

    response_json <-
        response |>
        content(as = "text", encoding = "UTF-8") |>
        parse_json()

    buys <-
        response_json |>
        pluck("lot") |>
        map_depth(1,
                  ~(pluck(.x, "buy") |>
                        flatten_df() |>
                        mutate(symbol = pluck(.x, "security", "symbol"),
                               buy_sell = "BUY")
                  ) |>
                      relocate(buy_sell, symbol)
        ) |>
        bind_rows()

    sells <-
        response_json |>
        pluck("lot") |>
        map_depth(1,
                  ~(pluck(.x, "sell") |>
                        flatten_df() |>
                        mutate(symbol = pluck(.x, "security", "symbol"),
                               buy_sell = "SELL")
                  ) |>
                      relocate(buy_sell, symbol)
        ) |>
        bind_rows()

    realized_gain_loss_df <- bind_rows(buys, sells)

    return(realized_gain_loss_df)
}

#' Get the open positions in your portfolio.
#'
#' @param account_number a user's M1 Finance account number.
#'
get_open_positions <- function(account_number){
    url <-
        sprintf(
            "https://api.apexclearing.com/taxman/api/v1/gainloss/%s/unrealized?fromdate=19000101&page=1&pagesize=10000",
            account_number
        )

    response <-
        GET(url,
            add_headers(
                Cookie = Sys.getenv("APEX_token")
            )
        )

    if(response$status_code != 200){
        stop(sprintf("ERROR: API connection was unsuccessful, status code %s. Are you sure your account number is correct?", response$status_code))
    }

    response_json <-
        response |>
        content(as = "text", encoding = "UTF-8") |>
        parse_json()


    open_positions <-
        suppressWarnings(
            response_json["lot"] |>
            map_depth(
                2, ~tibble(
                    "symbol" = .x[["security"]][["symbol"]],
                    "description" = .x[["security"]][["description"]] |> purrr::flatten_chr(),
                    "buy_sell" = "BUY",
                    "trade_date" = .x[["buy"]][["tradeDate"]],
                    "unit_price" = .x[["buy"]][["unitPrice"]],
                    "quantity" = .x[["buy"]][["quantity"]]
                )
            ) |>
            bind_rows() |>
            mutate(total_cost = unit_price * quantity) |>
            filter(unit_price > 0)
        )

    return(open_positions)
}
