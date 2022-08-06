#' Helper function to clean up code which generates realized gains and losses.
#'
#' @param json_response the API response in JSON format.
#' @param short_or_long one of either "short" or "long" for the term in which gains are evaluated.
#'
get_gain_loss_by_term <- function(json_response,
                                  short_or_long = c("short", "long")){
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
