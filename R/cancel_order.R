#' Cancel Specific Order by ID for Account
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the encrypted account ID, and the order ID, cancel
#' the specific order.
#'
#' @return Returns a message informing the user if cancellation
#'         was successful or if there was an error.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords order account ID cancel
#' @importFrom httr DELETE add_headers content status_code
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param account_number encrypted ID of the account (string).
#' @param order_id order ID for the order (numeric).
#'
cancel_order <- function(tokens,
                         account_number,
                         order_id) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(account_number) || !is.numeric(order_id)) { # nolint
    stop("Tokens must be a list, account number must be a string, and the order ID must be numeric.") # nolint
  }
  # Define URL
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", account_number, "/orders/", order_id) # nolint
  # Send GET request
  request <- httr::DELETE(url = url,
                          httr::add_headers(`accept` = "application/json",
                                            `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Print message informing user that order was successfully cancelled
    print("Order was successfully cancelled.")
    # Return NULL
    return(NULL)
    # If invalid response, throw error and inform user
  } else {
    stop("Error during API call - please check inputs and ensure access token is refreshed.") # nolint
  }
}
