#' Place Order for Specific Account
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the encrypted account ID, and the request body (JSON), place
#' the specific order. Note that due to the complexity of the orders
#' that can be created, currently this function allows maximum flexibility
#' by not cultivating an easier solution to building the request
#' body and assumes the user passes the appropriate JSON. As
#' a result, it is strongly encouraged to look at the documentation for how
#' to build the proper orders for programmatic execution and do robust testing
#' outside of market hours to ensure that when a live trade comes it
#' will be just as the user intended. The user of this function assumes
#' all risk that trades could not be executed exactly as intended as
#' the API and this package are still under active development.
#'
#' @return Returns a message informing the user if the order was successfully
#'         placed/created or if there was an error.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords order account place
#' @importFrom httr POST add_headers content status_code
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param account_number encrypted ID of the account (string).
#' @param request_body Valid request to API for placing an order (JSON).
#'
place_order <- function(tokens,
                        account_number,
                        request_body) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(account_number) || !inherits(request_body, "json")) { # nolint
    stop("Tokens must be a list, account number must be a string, and the request body must be JSON.") # nolint
  }
  # Define URL
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", account_number, "/orders") # nolint
  # Send GET request
  request <- httr::POST(url = url,
                        query = request_body,
                        httr::add_headers(`accept` = "application/json",
                                            `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Print message informing user that order was successfully placed/created
    print("Order was successfully placed/created.")
    # Return NULL
    return(NULL)
    # If invalid response, throw error and inform user
  } else {
    stop("Error during API call - please check inputs and ensure access token is refreshed.") # nolint
  }
}