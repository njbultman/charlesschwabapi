#' Get Market Hours
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, return a data frame containing information about the
#' market of interest and its specific hours of operation. By
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
#' @param markets markets of interest that are "equity", "option", "bond", "future", and/or "forex" (string or character vector).
#' @param date date you would like to get the hours of operation. Valid dates are today through one year from now (date).
#'
get_market_hours <- function(tokens,
                             markets = c("equity", "option", "bond", "future", "forex"), # nolint
                             date = NULL) {
  # Ensure tokens parameter is a list, markets is a character string/vector, and date is date # nolint
  if (!is.list(tokens) || !is.character(markets) || (!is.null(date) && !lubridate::is.Date(date))) { # nolint
    stop("Tokens parameter must be a list, markets should be a character vector or string, and date should be a date.") # nolint
  }
  # Ensure markets is one of the appropriate values
  if (length(setdiff(markets, c("equity", "option", "bond", "future", "forex")) > 0)) { # nolint
    stop("Markets must be 'equity', 'option', 'bond', 'future', and/or 'forex'.") # nolint
  }
  # Ensure date is one of the appropriate values
  if (!is.null(date) && (date < Sys.Date() || date > seq(Sys.Date(), length = 2, by = "1 years")[2])) { # nolint
    stop("Date must be today or no greater than one year in the future when not NULL.") # nolint
  }
  # Define URL for GET request
  url <- "https://api.schwabapi.com/marketdata/v1/markets"
  # Define query parameters
  query <- list("markets" = paste0(markets, collapse = ","),
                "date" = strftime(date, "%Y-%m-%d"))
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform each object in the list to a data frame but preserve list format
    req_list_df <- lapply(req_list, dplyr::bind_rows)
    # Bind all objects from list to a data frame
    req_df <- dplyr::bind_rows(req_list_df)
    # Return data frame
    return(req_df)
    # If invalid response returned (not 200), stop and inform user
  } else {
    stop("Error during API call - please check inputs and ensure access token is refreshed.") # nolint
  }
}