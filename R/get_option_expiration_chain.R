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
  # Extract status code from request
  request_status_code <- httr::status_code(request)
  # Try to extract content from request
  req_list <- tryCatch(
    expr = {
      httr::content(request)
    },
    # If error in extracting, throw generic error message and print request object (more specifics follow below) #nolint
    error = function(e) {
      message("Error during API call:")
      print(request)
    }
  )
  # Check if valid response returned (200)
  if (request_status_code == 200) {
    # Transform list to data frame
    req_df <- dplyr::bind_rows(req_list)
    # Parse expiration date to date field
    req_df$expirationDate <- as.Date(req_df$expirationDate, format = "%Y-%m-%d")
    # Return data frame
    return(req_df)
    # Otherwise, cycle through errors and print content from error rather than generic error like above #nolint
  } else if (request_status_code == 400) {
    message("400 error - validation problem with the request. Double check input objects, including tokens, and try again. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else if (request_status_code == 401) {
    message("401 error - authorization token is invalid. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else if (request_status_code == 404) {
    message("404 error - resource is not found. Double check inputs and try again later. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else if (request_status_code == 500) {
    message("500 error - unexpected server error. Please try again later. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else {
    # If another error is encountered that is not in documentation, inform/print it for the user #nolint
    message("Error during API call - more specifics are below: ")
    print(unlist(req_list))
  }
}
