#' Get Instruments
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the symbol(s) of interest, and the search type, return
#' a data frame with information about the securities matching
#' the search.
#'
#' @return Returns a data frame containing information surrounding
#' the symbol(s) of interest in the search.
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
  # Ensure tokens parameter is a list, symbol is character vector or string, and projection is a string #nolint
  if (!is.list(tokens) || !is.character(symbol) || !is.character(projection)) {
    stop("Tokens parameter must be a list, symbol should be a string or character vector, and projection should be strings.") #nolint
  }
  # Ensure projection is one of the appropriate values
  if (length(setdiff(projection, c("symbol-search", "symbol-regex", "desc-search", "desc-regex", "search", "fundamental")) > 0)) { # nolint
    stop("Projection must be 'symbol-search', 'symbol-regex', 'desc-search', 'desc-regex', 'search', or 'fundamental'.") # nolint
  }
  # Define URL for GET request
  url <- "https://api.schwabapi.com/marketdata/v1/instruments"
  # Define query parameters
  query <- list("symbol" = paste(symbol, collapse = ","),
                "projection" = projection)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response is returned (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Bind all objects from list to a data frame
    req_df <- dplyr::bind_rows(req_list)
    # Return data frame
    return(req_df)
    # If invalid response is returned, throw error and inform user
  } else {
    stop("Error during API call - please check token input object and ensure access token is refreshed.") # nolint
  }
}
