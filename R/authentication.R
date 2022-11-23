#' Get credentials to obtain an APEX auth token.
#'
#' @details
#' This function checks whether a session is interactive. If it is, it prompts the
#' user to enter their username and password. Otherwise, it checks .Renviron for the
#' credentials.
#'
#' @return
#' a named list with a user's credentials
get_credentials <- function() {
  if (interactive()) {
    username <- askForSecret("APEX Username")
    password <- askForSecret("APEX Password")
  }
  else {
    message("checking .Renviron for username and password")
    username <- Sys.getenv("APEX_USER")
    password <- Sys.getenv("APEX_PASS")
  }

  if (username == "" & password == ""){
    stop("could not find username and password. Please set up the following environment variables for non-interactive sessions: APEX_USER, APEX_PASS")
  }

  list(
    "username" = username,
    "password" = password
  )
}

#' Get an authorized token to use during your R session.
#'
#' A token is required in order to make requests, this token is hidden in the cookies generated when a user logs into their account.
#' This function uses the login service and grabs the token from the cookies returned when the request is successful.
#'
#' @details
#' If a user is uncomfortable writing out their username/password in an R file as plaintext, the function can be left blank and
#' RStudio will prompt the user to enter their username and password. This is not stored anywhere once the function is finished running - only the AUTH_token is.
#'
#' @return a global environment variable called APEX_token which is used in the other helper functions.
#' @examples
#' \dontrun{
#' get_APEX_auth_token() # RStudio prompts the user for their login
#' }
get_APEX_auth_token <- function() {
  credentials <- get_credentials()

  url <- "https://api.apexclearing.com/legit/api/v2/session"
  request_body <-
    paste0(
      '{"user":"","password":"',
      credentials$password,
      '","username":"',
      credentials$username,
      '"}'
    )

  response <-
    POST(
      url,
      add_headers(
        .headers = c("Content-Type" = "application/json;charset=UTF-8")
      ),
      body = request_body
    )

  if (response$status_code != 200) {
    stop(
      sprintf("ERROR: response returned status code %s. Check your credentials and try again.",
              response$status_code)
    )
  }

  token <-
    response |>
    pluck("cookies") |>
    filter(.data$name == "apex_jwt") |>
    pull(.data$value)

  Sys.setenv("APEX_token" = token)
}
