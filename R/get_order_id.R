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
#' @importFrom tibble
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param encrypted_account_id encrypted ID of the account from `get_account_numbers` function (string).
#' @param order_id order ID for the order (numeric).
#'
get_order_id <- function(tokens,
                         encrypted_account_id,
                         order_id) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(encrypted_account_id) || !is.numeric(order_id)) { # nolint
    stop("Tokens must be a list, encrypted account ID must be a string, and the order ID must be numeric.") # nolint
  }
  # Define URL
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", encrypted_account_id, "/orders/", order_id) # nolint
  # Send GET request
  request <- httr::GET(url = url,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Extract status code from request
  request_status_code <- httr::status_code(request)
  # Extract content from request
  req_list <- httr::content(request)
  # Check if valid response returned (200)
  if (request_status_code == 200) {
    # Transform list to data frame
    req_df <- as_tibble(do.call(cbind, req_list))
    # Return data frame
    return(req_df)
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
