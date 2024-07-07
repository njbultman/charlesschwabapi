#' Replace Order for Specific Account
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the encrypted account ID, the order ID and the
#' request body, replace the specific order. Due to the complexity of the orders
#' that can be created/replaced, currently this function allows
#' maximum flexibility by not cultivating an easier solution
#' to building the request body and assumes the user passes a
#' complex list in the exact format that, when parsed as JSON, will yield
#' the proper request body to the API. Much like the `place_order` function,
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
#' @keywords order account place
#' @importFrom httr PUT add_headers content status_code
#' @importFrom jsonlite toJSON
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param account_number encrypted ID of the account (string).
#' @param order_id order ID to be replaced (numeric).
#' @param request_body list object that when transformed to JSON will create a valid request to API for replacing an order (list).
#'
replace_order <- function(tokens,
                        account_number,
                        order_id,
                        request_body) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(account_number) || !is.numeric(order_id) || !is.list(request_body)) { # nolint
    stop("Tokens must be a list, account number must be a string, order ID must be numeric, and the request body must be a list.") # nolint
  }
  # Define URL
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", account_number, "/orders/", order_id) # nolint
  # Send GET request
  request <- httr::PUT(url = url,
                       query = jsonlite::toJSON(request_body),
                       httr::add_headers(`accept` = "application/json",
                                            `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Print message informing user that order was successfully replaced/created
    print("Order was successfully replaced/created.")
    # Return NULL
    return(NULL)
    # If invalid response, throw error and inform user
  } else {
    stop("Error during API call - please check inputs and ensure access token is refreshed.") # nolint
  }
}
