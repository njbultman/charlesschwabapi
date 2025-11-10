#' Get Price History
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the equity symbol of interest, return the recent price
#' history for the symbol. There are additional parameters that
#' can be specified to customize the call, but specific information
#' about the default value information is given below (both in this
#' description and the specifics on each parameter).
#'
#' While the parameter defaults for this function are NULL, the values
#' that ultimately feed into the API call are used behind-the-scenes.
#' For example, the period type parameter's default in the function is
#' NULL, but behind-the-scenes the API is using "day" to grab the
#' appropriate data.
#'
#' Additionally, there are defaults for parameters that are dependent on
#' other parameters. For the period parameter, if the period type is "day",
#' the default period is 10, otherwise it is 1. For the frequency type
#' parameter, if it is "day" then the default value is "minute", otherwise
#' it is "monthly" for year and "weekly" for the others.
#'
#' @return Returns a data frame containing information surrounding
#'         the price history of the symbol of interest.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords option expiration chain
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @importFrom anytime anytime
#' @importFrom lubridate is.POSIXt
#' @importFrom purrr keep
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param symbol equity symbol (string).
#' @param period_type chart period being requested. Can be "day", "month", "year", or "ytd". Default is NULL, which is "day" (string).
#' @param period number of chart period types. Default is NULL, which will then be dependent on the period type (numeric).
#' @param frequency_type the time frequency type. Default is NULL, which will then be dependent on the period type (string).
#' @param frequency the time frequency duration. Default is NULL, which is 1 (numeric).
#' @param start_datetime start datetime of request. Default is NULL, which is the end date less the period (datetime).
#' @param end_datetime end datetime of request. Default is NULL, which is the market close of the previous business day (datetime).
#' @param need_extended_hours_data indicator for whether extended hours data is needed. Default is NULL, which is TRUE (boolean).
#' @param need_previous_close indicator for whether the previous close price/date is needed. Default is NULL, which is FALSE (boolean).
#'
get_price_history <- function(tokens,
                              symbol,
                              period_type = NULL,
                              period = NULL,
                              frequency_type = NULL,
                              frequency = NULL,
                              start_datetime = NULL,
                              end_datetime = NULL,
                              need_extended_hours_data = NULL,
                              need_previous_close = NULL) {
  # Ensure tokens parameter is a list, symbol and period/frequency type are strings, and period/frequency are numeric
  if (!is.list(tokens) || !is.character(symbol) || (!is.null(period_type) && !is.character(period_type)) || (!is.null(period) && !is.numeric(period)) || (!is.null(frequency_type) && !is.character(frequency_type)) || (!is.null(frequency) && !is.numeric(frequency))) {
    stop("Tokens parameter must be a list and symbol must be a string. Also, period type and frequency type must be numeric or NULL, and period and frequency must be numeric or NULL.")
  }
  # Ensure start/end dates are datetimes or NULL and extended hours and previous close are boolean or NULL
  if ((!is.null(start_datetime) && !lubridate::is.POSIXt(start_datetime)) || (!is.null(end_datetime) && !lubridate::is.POSIXct(end_datetime))  || (!is.null(need_extended_hours_data) && !is.logical(need_extended_hours_data)) || (!is.null(need_previous_close) && !is.logical(need_previous_close))) {
    stop("Start/end dates must be a date and extended hours and previous close must be boolean.")
  }
  # Ensure period type is NULL or "day", "month", "year" or "ytd"
  if (!is.null(period_type) && (length(setdiff(period_type, c("day", "month", "year", "ytd")) > 0))) {
    stop("Period type must be NULL or 'day', 'month', 'year', or 'ytd'.")
  }
  # If period type is NULL or "day" then ensure period value is NULL, 1, 2, 3, 4, 5, or 10
  if ((is.null(period_type) || period_type == "day") && (!is.null(period) && length(setdiff(period, c(1, 2, 3, 4, 5, 10)) > 0))) {
    stop("If period type is NULL or 'day' then period must be NULL, 1, 2, 3, 4, 5, or 10.")
  }
  # If period type is "month" then ensure period value is NULL, 1, 2, 3, 4, 5, or 10
  if ((period_type == "month") && (!is.null(period) && length(setdiff(period, c(1, 2, 3, 4, 6)) > 0))) {
    stop("If period type is 'month' then period must be NULL, 1, 2, 3, or 6.")
  }
  # If period type is "year" then ensure period value is NULL, 1, 2, 3, 5, 10, 15, or 20
  if ((period_type == "year") && (!is.null(period) && length(setdiff(period, c(1, 2, 3, 5, 10, 15, 20)) > 0))) {
    stop("If period type is 'year' then period must be NULL, 1, 2, 3, 5, 10, 15, or 20.")
  }
  # If period type is "ytd" then ensure period value is NULL or 1
  if ((period_type == "ytd") && (!is.null(period) && length(setdiff(period, c(1)) > 0))) {
    stop("If period type is 'ytd' then period must be NULL or 1.")
  }
  # If period type is NULL or "day" then ensure frequency type is "minute"
  if ((is.null(period_type) || period_type == "day") && (!is.null(frequency_type) && length(setdiff(frequency_type, c("minute")) > 0))) {
    stop("If period type is NULL or 'day' then frequency type must be 'minute'.")
  }
  # If period type is "month" then ensure frequency type is "daily" or "weekly"
  if ((period_type == "month") && (!is.null(frequency_type) && length(setdiff(frequency_type, c("daily", "weekly")) > 0))) {
    stop("If period type is 'month' then frequency type must be 'daily', 'weekly'.")
  }
  # If period type is "year" then ensure frequency type is "daily", "weekly", or "monthly"
  if ((period_type == "year") && (!is.null(frequency_type) && length(setdiff(frequency_type, c("daily", "weekly", "monthly")) > 0))) {
    stop("If period type is 'year' then frequency type must be 'daily' or 'weekly', or 'monthly'.")
  }
  # If period type is "ytd" then ensure frequency type is "daily" or "weekly"
  if ((period_type == "ytd") && (!is.null(frequency_type) && length(setdiff(frequency_type, c("daily", "weekly")) > 0))) {
    stop("If period type is 'ytd' then frequency type must be 'daily', 'weekly'.")
  }
  # If frequency is not NULL and frequency type is "minute" then ensure frequency is NULL, 1, 5, 10, 15, or 30
  if (!is.null(frequency) && frequency_type == "minute") {
    stop("Frequency must be NULL, 1, 5, 10, 15, or 30 when frequency type is 'minute'.")
  }
  # If frequency is not NULL and frequency type is "daily", "weekly", or "monthly" then ensure frequency is NULL or 1
  if (!is.null(frequency) && frequency != 1 && (frequency_type == "daily" || frequency_type == "weekly" || frequency_type == "monthly")) {
    stop("Frequency must be NULL or 1 when frequency type is 'daily', 'weekly', or 'monthly'.")
  }
  # Define URL for GET request
  url <- "https://api.schwabapi.com/marketdata/v1/pricehistory"
  # Define list to hold error messages
  error_messages <- list(
    "400" = "400 error - validation problem with the request. Double check input objects, including tokens, and try again.",
    "401" = "401 error - authorization token is invalid.",
    "404" = "404 error - resource is not found. Double check inputs and try again later.",
    "500" = "500 error - unexpected server error. Please try again later."
  )
  # Define query parameters
  query <- list("symbol" = symbol,
                "periodType" = period_type,
                "period" = period,
                "freqencyType" = frequency_type,
                "frequency" = frequency,
                "startDate" = as.numeric(start_datetime) * 1000,
                "endDate" = as.numeric(end_datetime) * 1000,
                "needExtendedHoursData" = need_extended_hours_data,
                "needPreviousClose" = need_previous_close)
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
    # Only keep elements that have one value (these will be appended to final data frame later)
    req_list_subset <- purrr::keep(req_list, function(x) length(x) == 1)
    # Transform these elements into their own data frame
    req_list_subset_df <- data.frame(req_list_subset)
    # Transform candles list to data frame
    req_df <- dplyr::bind_rows(req_list$candles)
    # If no records in data frame, inform user and do not parse modify datetime
    if (nrow(req_df) == 0) {
      message(paste0("No data found for '", symbol, "'. Is it spelled correctly?"))
    } else {
      # If records, switch datetime column from unix to interpretable datetime
      req_df$datetime <- anytime::anytime(req_df$datetime / 1000)
    }
    # Add columns that only contain one value (subsetted in the beginning)
    for (i in names(req_list_subset_df)) {
      req_df[paste0(i)] <- req_list_subset_df[paste0(i)]
    }
    # If previousCloseDate column is present, parse it to interpratable datetime
    if (!is.na(match("previousCloseDate", names(req_df)))) {
        req_df$previousCloseDate <- anytime::anytime(req_df$previousCloseDate / 1000)
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
