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
  # Define list to hold error messages
  error_messages <- list(
    "400" = "400 error - validation problem with the request. Double check input objects, including tokens, and try again.",
    "401" = "401 error - authorization token is invalid.",
    "404" = "404 error - resource is not found. Double check inputs and try again later.",
    "500" = "500 error - unexpected server error. Please try again later."
  )
  # Define query parameters
  query <- list("symbol" = symbol)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token)))
  # Extract status code from request as string
  request_status_code <- as.character(httr::status_code(request))
  # Check if valid response returned (200)
  if (request_status_code == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform list to data frame
    req_df <- dplyr::bind_rows(req_list)
    # If no records in data frame, inform user and do not parse expiration date
    if (nrow(req_df) == 0) {
      message(paste0("No data found for '", symbol, "'. Is it spelled correctly?"))
    } else {
      # Parse expiration date to date field
      req_df$expirationDate <- as.Date(req_df$expirationDate, format = "%Y-%m-%d")
    }
    # Return data frame
    return(req_df)
    # If API call is not a good status code
  } else {
    # Get appropriate error message
    error_message <- error_messages[request_status_code]
    # If cannot find any error message, set to generic message
    if (is.null(error_message)) {
      error_message <- "Error during API call."
    }
    # Print error message and details from call
    message(paste(error_message, "More details are below:"))
    print(unlist(request))
  }
}
