#' Get Orders for Specific Account
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the encrypted account ID, return a data frame
#' containing the orders for the specific account.
#' By default, it will return all orders (default max is 3000)
#' from the last 60 days. This can be adjusted through
#' additional function parameters.
#'
#' The easiest way to enter the right strings in the start/end times is to
#' begin with `Sys.time()`, use the `lubridate` package to adjust it, and
#' then format it as `%Y-%m-%dT%H:%M:%S.000Z`. For example, to get the current
#' date less 30 days, you can use:
#' `strftime(Sys.time() - lubridate::days(30), format = "%Y-%m-%dT%H:%M:%S.000Z")`.
#' The defaults for this function illustrate the appropriate usage for the API.
#' 
#' @return Returns a data frame containing order information
#'         for the specified account.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords orders account
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param encrypted_account_id encrypted ID of the account from `get_account_numbers` function (string).
#' @param from_entered_datetime start of time range for orders that should be gathered - default is current datetime less 60 days (string).
#' @param to_entered_datetime end of time range for orders that should be gathered - default is current datetime (string).
#' @param max_results maximum number of results to be returned - default is NULL, which is 3000 (numeric).
#' @param status only orders of this status should be returned. Default is NULL, which is all statuses. Valid values are "AWAITING_PARENT_ORDER", "AWAITING_CONDITION", "AWAITING_STOP_CONDITION", "AWAITING_MANUAL_REVIEW", "ACCEPTED", "AWAITING_UR_OUT", "PENDING_ACTIVATION", "QUEUED", "WORKING", "REJECTED", "PENDING_CANCEL", "CANCELED", "PENDING_REPLACE", "REPLACED", "FILLED", "EXPIRED", "NEW", "AWAITING_RELEASE_TIME", "PENDING_ACKNOWLEDGEMENT", "PENDING_RECALL", and "UNKNOWN" (string).
#'
get_orders_account <- function(tokens,
                               encrypted_account_id,
                               from_entered_datetime = strftime(Sys.time() - lubridate::days(60), format = "%Y-%m-%dT%H:%M:%S.000Z"),
                               to_entered_datetime = strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%S.000Z"),
                               max_results = NULL,
                               status = NULL) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(encrypted_account_id) || !is.character(from_entered_datetime) || !is.character(to_entered_datetime) || (!is.null(max_results) && !is.numeric(max_results)) || (!is.null(status) && !is.character(status))) {
    stop("Tokens must be a list, encrypted account ID, from/to entered times, and status must be strings, and max results should be numeric.")
  }
  # Ensure status is NULL or a valid value
  if (!is.null(status) && (length(setdiff(status, c("AWAITING_PARENT_ORDER", "AWAITING_CONDITION", "AWAITING_STOP_CONDITION", "AWAITING_MANUAL_REVIEW", "ACCEPTED", "AWAITING_UR_OUT", "PENDING_ACTIVATION", "QUEUED", "WORKING", "REJECTED", "PENDING_CANCEL", "CANCELED", "PENDING_REPLACE", "REPLACED", "FILLED", "EXPIRED", "NEW", "AWAITING_RELEASE_TIME", "PENDING_ACKNOWLEDGEMENT", "PENDING_RECALL", "UNKNOWN")) > 0))) {
    stop("Status must be NULL or 'AWAITING_PARENT_ORDER', 'AWAITING_CONDITION', 'AWAITING_STOP_CONDITION', 'AWAITING_MANUAL_REVIEW', 'ACCEPTED', 'AWAITING_UR_OUT', 'PENDING_ACTIVATION', 'QUEUED', 'WORKING', 'REJECTED', 'PENDING_CANCEL', 'CANCELED', 'PENDING_REPLACE', 'REPLACED', 'FILLED', 'EXPIRED', 'NEW', 'AWAITING_RELEASE_TIME', 'PENDING_ACKNOWLEDGEMENT', 'PENDING_RECALL', or 'UNKNOWN'.")
  }
  # Define URL
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", encrypted_account_id, "/orders")
  # Define list to hold error messages
  error_messages <- list(
    "400" = "400 error - validation problem with the request. Double check input objects, including tokens, and try again.",
    "401" = "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application.",
    "403" = "403 error - caller is forbidden from accessing this service.",
    "404" = "404 error - resource is not found. Double check inputs and try again later.",
    "500" = "500 error - unexpected server error. Please try again later.",
    "503" = "503 error - server has a temporary problem responding. Please try again later."
  )
  # Define payload
  query <- list("maxResults" = max_results,
                "fromEnteredTime" = from_entered_datetime,
                "toEnteredTime" = to_entered_datetime,
                "status" = status)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token)))
  # Extract status code from request as string
  request_status_code <- as.character(httr::status_code(request))
  # Extract content from request
  req_list <- httr::content(request)
  # Check if valid response returned (200)
  if (request_status_code == 200) {
    # Transform list to data frame
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
    print(unlist(req_list))
  }
}
