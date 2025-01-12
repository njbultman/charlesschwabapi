#' Replace Order for Specific Account
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the encrypted account ID, and the order ID and the
#' request body, replace the specific order. Due to the complexity of the orders
#' that can be created/replaced, currently this function allows
#' maximum flexibility by not cultivating an easier solution
#' to building the request body and assumes the user passes
#' the appropriate JSON. Much like the `place_order` function,
#' it is strongly encouraged to look at the documentation (in this package and
#' on the Charles Schwab developer site) for how to build proper orders before
#' attempting to replace any. The user of this function assumes
#' all risk that trades could not be replaced (and then executed)
#' exactly as intended.
#'
#' @return Returns a message informing the user if the order was successfully
#'         replaced/created or if there was an error.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords order account replace
#' @importFrom httr PUT add_headers content status_code
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param encrypted_account_id encrypted ID of the account from `get_account_numbers` function (string).
#' @param order_id order ID to be replaced (numeric).
#' @param request_body valid request to API for replacing an order (JSON).
#'
replace_order <- function(tokens,
                          encrypted_account_id,
                          order_id,
                          request_body) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(encrypted_account_id) || !is.numeric(order_id) || !inherits(request_body, "json")) { # nolint
    stop("Tokens must be a list, encrypted account ID must be a string, and the request body must be JSON.") # nolint
  }
  # Define URL
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", encrypted_account_id, "/orders/", order_id) # nolint
  # Send PUT request
  request <- httr::PUT(url = url,
                       query = request_body,
                       httr::add_headers(`Content-Type` = "application/json",
                                            `Authorization` = paste0("Bearer ", tokens$access_token)), # nolint
                       encode = "json")
  # Extract status code from request
  request_status_code <- httr::status_code(request)
  # Extract content from request
  req_list <- httr::content(request)
  # Check if valid response returned (200)
  if (request_status_code == 200) {
    # Inform user that order was successfully replaced/created
    message("Order was successfully replaced/created.")
    # Return NULL
    return(NULL)
    # If API call is not a good status code, go through other error codes called out in documentation and print error for user #nolint
  } else if (request_status_code == 400) {
    message("400 error - validation problem with the request. Double check input objects, including tokens, and try again. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else if (request_status_code == 401) {
    message("401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else if (request_status_code == 403) {
    message("403 error - caller is forbidden from accessing this service. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else if (request_status_code == 404) {
    message("404 error - resource is not found. Double check inputs and try again later. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else if (request_status_code == 500) {
    message("500 error - unexpected server error. Please try again later. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else if (request_status_code == 503) {
    message("503 error - server has a temporary problem responding. Please try again later. ", #nolint
            "More specifics on error are below:")
    print(unlist(req_list))
  } else {
    # If another error is encountered that is not in documentation, inform/print it for the user #nolint
    message("Error during API call - more specifics are below: ")
    print(unlist(req_list))
  }
}
