#' Get Specific Order by ID for Account
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the encrypted account ID, and the order ID, return
#' a data frame containing information about the specific order.
#'
#' @return Returns a data frame containing the information about
#'         the specific order.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords order account ID
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param account_number encrypted ID of the account (string).
#' @param order_id order ID for the order (numeric).
#'
get_order_id <- function(tokens,
                         account_number,
                         order_id) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(account_number) || !is.numeric(order_id)) { # nolint
    stop("Tokens must be a list, account number must be a string, and the order ID must be numeric.") # nolint
  }
  # Define URL
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", account_number, "/orders/", order_id) # nolint
  # Send GET request
  request <- httr::GET(url = url,
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
