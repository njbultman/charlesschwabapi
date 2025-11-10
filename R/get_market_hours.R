#' Get Market Hours
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, return a data frame containing information about the
#' market(s) of interest and the specific hours of operation. By
#' default, all of the markets are returned for today's date, but
#' both the specific markets returned and the date can be tweaked.
#' Please see the parameters for more specifics related to what can
#' be specified for the function.
#'
#' @return Returns a data frame containing information surrounding
#'         the market of interest and its specific hours of operation.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords market hours
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @importFrom lubridate is.Date
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param markets markets of interest that are "equity", "option", "bond", "future", and/or "forex". Default is all markets (string or character vector).
#' @param date date you would like to get the hours of operation. Valid dates are today through one year from now. Default is NULL, which is today (date).
#'
get_market_hours <- function(tokens,
                             markets = c("equity", "option", "bond", "future", "forex"),
                             date = NULL) {
  # Ensure tokens parameter is a list, markets is a character string/vector, and date is date
  if (!is.list(tokens) || !is.character(markets) || (!is.null(date) && !lubridate::is.Date(date))) {
    stop("Tokens parameter must be a list, markets should be a character vector or string, and date should be a date.")
  }
  # Ensure markets is one of the appropriate values
  if (length(setdiff(markets, c("equity", "option", "bond", "future", "forex")) > 0)) {
    stop("Markets must be 'equity', 'option', 'bond', 'future', and/or 'forex'.")
  }
  # Ensure date is one of the appropriate values
  if (!is.null(date) && (date < Sys.Date() || date > seq(Sys.Date(), length = 2, by = "1 years")[2])) {
    stop("Date must be today or no greater than one year in the future when not NULL.")
  }
  # Define URL for GET request
  url <- "https://api.schwabapi.com/marketdata/v1/markets"
  # Define list to hold error messages
  error_messages <- list(
    "400" = "400 error - validation problem with the request. Double check input objects, including tokens, and try again.",
    "401" = "401 error - authorization token is invalid.",
    "500" = "500 error - unexpected server error. Please try again later."
  )
  # Define query parameters
  query <- list("markets" = paste0(markets, collapse = ","),
                "date" = strftime(date, "%Y-%m-%d"))
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
    # Transform each object in the list to a data frame but preserve list format
    req_list_df <- lapply(req_list, dplyr::bind_rows)
    # Bind all objects from list to a data frame
    req_df <- dplyr::bind_rows(req_list_df)
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
