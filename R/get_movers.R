#' Get Movers
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the symbol of interest, return the top 10 securities
#' movement for a specific index in a data frame. By default, it is
#' sorted by volume and the frequency is 0, but these can be tweaked.
#'
#' @return Returns a data frame containing information surrounding
#'         the top 10 securities movement for the symbol specified.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords movers
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param symbol_id symbol of interest to get movers from. Valid values are "$DJI", "$COMPX", "$SPX", "NYSE", "NASDAQ", "OTCBB", "INDEX_ALL", "EQUITY_ALL", "OPTION_ALL", "OPTION_PUT", and "OPTION_CALL" (string).
#' @param sort the attribute that you would like sorted by. Valid values are "VOLUME", "TRADES", "PERCENT_CHANGE_UP", and "PERCENT_CHANGE_DOWN". Default is NULL, which is VOLUME (string).
#' @param frequency to return movers with hthe specified directions of up or down. Valid values are 0, 1, 5, 10, 30, or 60. Default is NULL, which is zero (numeric).
#'
get_movers <- function(tokens, symbol_id, sort = NULL, frequency = NULL) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(symbol_id) || (!is.null(sort) && !is.character(sort)) || (!is.null(frequency) && !is.numeric(frequency))) { # nolint
    stop("Tokens parameter must be a list, symbol and sort must be a strings, and frequency must be numeric.") # nolint
  }
  # Ensure symbol ID is one of the appropriate values
  if (length(setdiff(symbol_id, c("$DJI", "$COMPX", "$SPX", "NYSE", "NASDAQ", "OTCBB", "INDEX_ALL", "EQUITY_ALL", "OPTION_ALL", "OPTION_PUT", "OPTION_CALL")) > 0)) { # nolint
    stop("Symbol ID must be '$DJI', '$COMPX', '$SPX', 'NYSE', 'NASDAQ', 'OTCBB', 'INDEX_ALL', 'EQUITY_ALL', 'OPTION_ALL', 'OPTION_PUT', or 'OPTION_CALL'.") # nolint
  }
  # Ensure sort is one of the appropriate values
  if (!is.null(sort) && (length(setdiff(sort, c("VOLUME", "TRADES", "PERCENT_CHANGE_UP", "PERCENT_CHANGE_DOWN")) > 0))) { # nolint
    stop("Sort must be 'VOLUME', 'TRADES', 'PERCENT_CHANGE_UP', or 'PERCENT_CHANGE_DOWN'.") # nolint
  }
  # Ensure frequency is one of the appropriate values
  if (!is.null(frequency) && (length(setdiff(frequency, c(0, 1, 5, 10, 30, 60)) > 0))) { # nolint
    stop("Frequency must be 0, 1, 5, 10, 30, or 60.") # nolint
  }
  # Define URL for GET request
  url <- paste0("https://api.schwabapi.com/marketdata/v1/movers/", symbol_id)
  # Define query parameters
  query <- list("sort" = sort,
                "frequency" = frequency)
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
