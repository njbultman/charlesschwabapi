#' Get Option Expiration Chain
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the symbol of interest, return the option expiration
#' chain information related to the symbol. This includes expiration
#' dates, expiration types, settlement types, and more.
#'
#' @return Returns a data frame containing information surrounding
#'         the option expiration chain.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords option expiration chain
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param symbol symbol for option expiration chain (string).
#'
get_option_expiration_chain <- function(tokens, symbol) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(symbol)) {
    stop("Tokens parameter must be a list and symbol parameter must be a string.")
  }
  # Define URL for GET request
  url <- "https://api.schwabapi.com/marketdata/v1/expirationchain"
  # Define query parameters
  query <- list("symbol" = symbol)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform list to data frame
    req_df <- dplyr::bind_rows(req_list)
    # Parse expiration date to date field
    req_df$expirationDate <- as.Date(req_df$expirationDate, format = "%Y-%m-%d")
    # Return data frame
    return(req_df)
    # If invalid response, stop function and inform user
  } else {
    stop("Error during call - please check inputs and ensure access token is refreshed.") # nolint
  }
}