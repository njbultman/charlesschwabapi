#' Replace Order for Specific Account
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the encrypted account ID, the order ID and the
#' request body, replace the specific order. Due to the complexity of the orders
#' that can be created/replaced, currently this function allows
#' maximum flexibility by not cultivating an easier solution
#' to building the request body and assumes the user passes
#' the appropriate JSON. Much like the `place_order` function,
#' it is strongly encouraged to look at the documentation (in this package and
#' on the Charles Schwab developer site) for how to build proper orders before
#' attempting to replace any. The user of this function assumes
#' all risk that trades could not be replaced (and then executed)
#' exactly as intended as the API and this package are still under
#' active development.
#'
#' @return Returns a message informing the user if the order was successfully
#'         replaced/created or if there was an error.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords order account replace
#' @importFrom httr PUT add_headers content status_code
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param account_number encrypted ID of the account (string).
#' @param order_id order ID to be replaced (numeric).
#' @param request_body Valid request to API for replacing an order (JSON).
#'
replace_order <- function(tokens,
                          account_number,
                          order_id,
                          request_body) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(account_number) || !is.numeric(order_id) || !inherits(request_body, "json")) { # nolint
    stop("Tokens must be a list, account number must be a string, and the request body must be JSON.") # nolint
  }
  # Define URL
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", account_number, "/orders/", order_id) # nolint
  # Send GET request
  request <- httr::PUT(url = url,
                       query = request_body,
                       httr::add_headers(`accept` = "application/json",
                                            `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Inform user that order was successfully replaced/created
    message("Order was successfully replaced/created.")
    # Return NULL
    return(NULL)
    # If invalid response, throw error and inform user
  } else {
    stop("Error during API call - please check inputs and ensure access token is refreshed.") # nolint
  }
}
