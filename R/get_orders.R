#' Get Orders
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, return a data frame containing the orders for all
#' accounts related to the authenticated user. By default, it
#' will return all orders (default max is 3000) from the last 60 days.
#' This can be adjusted through additional function parameters.
#'
#' @return Returns a data frame containing order information for all
#'         accounts affiliated with authorized user.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords orders
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param from_entered_datetime encrypted ID of the account - default is current datetime less 60 days (string).
#' @param to_entered_datetime specific fields to be returned - default is current datetime (string).
#' @param max_results maximum number of results to be returned - default is NULL, which is 3000 (numeric).
#' @param status only orders of this status should be returned. Default is NULL, which is all statuses. Valid values are "AWAITING_PARENT_ORDER", "AWAITING_CONDITION", "AWAITING_STOP_CONDITION", "AWAITING_MANUAL_REVIEW", "ACCEPTED", "AWAITING_UR_OUT", "PENDING_ACTIVATION", "QUEUED", "WORKING", "REJECTED", "PENDING_CANCEL", "CANCELED", "PENDING_REPLACE", "REPLACED", "FILLED", "EXPIRED", "NEW", "AWAITING_RELEASE_TIME", "PENDING_ACKNOWLEDGEMENT", "PENDING_RECALL", and "UNKNOWN" (string).
#'
get_orders <- function(tokens,
                       from_entered_datetime = strftime(Sys.time() - lubridate::days(60), format = "%Y-%m-%dT%H:%M:%S.000Z"), # nolint
                       to_entered_datetime = strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%S.000Z"), # nolint
                       max_results = NULL,
                       status = NULL) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(from_entered_datetime) || !is.character(to_entered_datetime) || (!is.null(max_results) && !is.numeric(max_results)) || (!is.null(status) && !is.character(status))) { # nolint
    stop("Tokens must be a list, from/to entered times and status must be strings, and max results should be numeric.") # nolint
  }
  # Ensure status is NULL or a valid value
  if (!is.null(status) && (length(setdiff(status, c("AWAITING_PARENT_ORDER", "AWAITING_CONDITION", "AWAITING_STOP_CONDITION", "AWAITING_MANUAL_REVIEW", "ACCEPTED", "AWAITING_UR_OUT", "PENDING_ACTIVATION", "QUEUED", "WORKING", "REJECTED", "PENDING_CANCEL", "CANCELED", "PENDING_REPLACE", "REPLACED", "FILLED", "EXPIRED", "NEW", "AWAITING_RELEASE_TIME", "PENDING_ACKNOWLEDGEMENT", "PENDING_RECALL", "UNKNOWN")) > 0))) { # nolint
    stop("Status must be NULL or 'AWAITING_PARENT_ORDER', 'AWAITING_CONDITION', 'AWAITING_STOP_CONDITION', 'AWAITING_MANUAL_REVIEW', 'ACCEPTED', 'AWAITING_UR_OUT', 'PENDING_ACTIVATION', 'QUEUED', 'WORKING', 'REJECTED', 'PENDING_CANCEL', 'CANCELED', 'PENDING_REPLACE', 'REPLACED', 'FILLED', 'EXPIRED', 'NEW', 'AWAITING_RELEASE_TIME', 'PENDING_ACKNOWLEDGEMENT', 'PENDING_RECALL', or 'UNKNOWN'.") # nolint
  }
  # Define URL
  url <- "https://api.schwabapi.com/trader/v1/orders"
  # Define payload
  query <- list("maxResults" = max_results,
                "fromEnteredTime" = from_entered_datetime,
                "toEnteredTime" = to_entered_datetime,
                "status" = status)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform list to data frame
    req_df <- dplyr::bind_rows(req_list)
    # Return data frame
    return(req_df)
    # If invalid response, throw error and inform user
  } else {
    stop("Error during API call - please check inputs and ensure access token is refreshed.") # nolint
  }
}
