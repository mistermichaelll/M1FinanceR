#' Get an authorized token to use during your R session.
#'
#' A token is required in order to make requests, this token is hidden in the cookies generated when a user logs into their account.
#' This function uses the login service and grabs the token from the cookies returned when the request is successful.
#'
#' @details
#' If a user is uncomfortable writing out their username/password in an R file as plaintext, the function can be left blank and
#' RStudio will prompt the user to enter their username and password. This is not stored anywhere once the function is finished running - only the AUTH_token is.
#'
#' @param username the username for your APEX Clearing account.
#' @param password the password for your APEX Clearing account.
#' @return a global environment variable called APEX_token which is used in the other helper functions.
#' @examples
#' get_APEX_auth_token(username = "username", password = "password")
#' get_APEX_auth_token() # RStudio prompts the user for their login
get_APEX_auth_token <- function(username, password){
  if(missing(username) | missing(password)){
    username <- rstudioapi::askForSecret("APEX Username")
    password <- rstudioapi::askForSecret("APEX Password")
  }

  url <- "https://api.apexclearing.com/legit/api/v2/session"
  request_body <-
    paste0(
      '{"user":"","password":"',
      password,
      '","username":"',
      username,
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
    filter(name == "apex_jwt") |>
    pull(value)

  Sys.setenv("APEX_token" = token)
}
