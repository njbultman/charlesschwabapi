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
#' @return Returns a numeric order number and a message informing the user if the order was successfully
#'         placed/created or if there was an error.
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
  if (!is.list(tokens) || !is.character(encrypted_account_id) || !is.numeric(order_id) || !inherits(request_body, "json")) {
    stop("Tokens must be a list, encrypted account ID must be a string, and the request body must be JSON.")
  }
  # Define URL
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", encrypted_account_id, "/orders/", order_id)
  # Define list to hold error messages
  error_messages <- list(
    "400" = "400 error - validation problem with the request. Double check input objects, including tokens, and try again.",
    "401" = "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application.",
    "403" = "403 error - caller is forbidden from accessing this service.",
    "404" = "404 error - resource is not found. Double check inputs and try again later.",
    "500" = "500 error - unexpected server error. Please try again later.",
    "503" = "503 error - server has a temporary problem responding. Please try again later."
  )
  # Send PUT request
  request <- httr::PUT(url = url,
                       body = request_body,
                       httr::add_headers(`Content-Type` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token)), 
                       encode = "json")
  # Extract status code from request as string
  request_status_code <- as.character(httr::status_code(request))
  # Extract content from request
  req_list <- httr::content(request)
  # Check if valid response returned (201)
  if (request_status_code == 201) {
    # Inform user that order was successfully replaced/created
    message("Order was successfully replaced/created.")
    # Get the order number
    order_num <- sub(paste0("https://api.schwabapi.com/trader/v1/accounts/",
                            encrypted_account_id,
                            "/orders/"),
                     "",
                     request$headers$location)
    # Return the order number
    return(as.numeric(order_num))
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
