#' Get Market Hours for Single Market
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the market of interest, return specific information
#' about that market's hours of operation. Note that a data parameter
#' can also be specified between now and one year from now to obtain
#' specific hours of operation for the market related to that date.
#'
#' @return Returns a data frame containing information surrounding
#'         the market's hours of operation.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords market hours operation single
#' @importFrom httr GET add_headers content status_code
#' @importFrom lubridate is.Date
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param market market of interest that is either "equity", "option", "bond", "future", or "forex" (string).
#' @param date date you would like to get the hours of operation. Valid dates are today through one year from now. Default is NULL, which is today (date).
#'
get_market_hours_single <- function(tokens, market, date = NULL) {
  # Ensure tokens parameter is a list, markets is a character string/vector, and date is date #nolint
  if (!is.list(tokens) || !is.character(market) || (!is.null(date) && !lubridate::is.Date(date))) { #nolint
    stop("Tokens parameter must be a list, market should be a string, and date should be a date.") #nolint
  }
  # Ensure market is one of the appropriate values
  if (length(setdiff(market, c("equity", "option", "bond", "future", "forex")) > 0)) { # nolint
    stop("Market must be 'equity', 'option', 'bond', 'future', or 'forex'.") # nolint
  }
  # Ensure date is one of the appropriate values
  if (!is.null(date) && (date < Sys.Date() || date > seq(Sys.Date(), length = 2, by = "1 years")[2])) { # nolint
    stop("Date must be today or no greater than one year in the future when not NULL.") # nolint
  }
  # Define URL for GET request
  url <- paste0("https://api.schwabapi.com/marketdata/v1/markets/", market)
  # Define query parameters
  query <- list("date" = strftime(date, "%Y-%m-%d"))
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
  # Check if valid response is returned (200)
  if (request_status_code == 200) {
    # Transform each object in the list to a data frame but preserve list format
    req_list_df <- lapply(req_list, dplyr::bind_rows)
    # Bind all objects from list to a data frame
    req_df <- dplyr::bind_rows(req_list_df)
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
