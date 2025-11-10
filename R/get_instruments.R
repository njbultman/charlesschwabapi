#' Get Instruments
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the symbol(s) of interest, and the search type, return
#' a data frame with information about the securities matching
#' the search.
#'
#' @return Returns a data frame containing information surrounding
#'         the symbol(s) of interest in the search.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords instrument instruments search
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param symbol symbol(s) of interest to search for (string or character vector).
#' @param projection type of search to be performed. Valid values are "symbol-search", "symbol-regex", "desc-search", "desc-regex", "search", and "fundamental" (string).
#'
get_instruments <- function(tokens, symbol, projection) {
  # Ensure tokens parameter is a list, symbol is character vector or string, and projection is a string
  if (!is.list(tokens) || !is.character(symbol) || !is.character(projection)) {
    stop("Tokens parameter must be a list, symbol should be a string or character vector, and projection should be strings.")
  }
  # Ensure projection is one of the appropriate values
  if (length(setdiff(projection, c("symbol-search", "symbol-regex", "desc-search", "desc-regex", "search", "fundamental")) > 0)) {
    stop("Projection must be 'symbol-search', 'symbol-regex', 'desc-search', 'desc-regex', 'search', or 'fundamental'.")
  }
  # Define URL for GET request
  url <- "https://api.schwabapi.com/marketdata/v1/instruments"
  # Define list to hold error messages
  error_messages <- list(
    "400" = "400 error - validation problem with the request. Double check input objects, including tokens, and try again.",
    "401" = "401 error - authorization token is invalid.",
    "500" = "500 error - unexpected server error. Please try again later."
  )
  # Define query parameters
  query <- list("symbol" = paste(symbol, collapse = ","),
                "projection" = projection)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token)))
  # Extract status code from request as string
  request_status_code <- as.character(httr::status_code(request))
  # If content can be extracted, check if valid response returned (200)
  if (request_status_code == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Bind all objects from list to a data frame
    req_df <- dplyr::bind_rows(req_list)
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
