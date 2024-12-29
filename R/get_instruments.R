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
  # If content can be extracted, check if valid response returned (200)
  if (request_status_code == 200) {
    # Bind all objects from list to a data frame
    req_df <- dplyr::bind_rows(req_list)
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
