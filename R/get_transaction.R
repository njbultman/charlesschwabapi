#' Get Specific Transaction Information
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the encrypted account ID of interest, and the
#' transaction ID of interest, return information corresponding
#' to that transaction. Note that the transaction ID can be obtained
#' by first calling the `get_transactions` function, finding the transaction
#' of interest, and then finding the transaction ID on that data frame.
#'
#' @return Returns a data frame containing the transaction
#'         information.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords account transaction
#' @importFrom httr GET add_headers content status_code
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param encrypted_account_id encrypted ID of the account from `get_account_numbers` function (string).
#' @param transaction_id transaction ID of interest (numeric).
#'
get_transaction <- function(tokens,
                            encrypted_account_id,
                            transaction_id) {
  # Ensure tokens parameter is a list, start/end dates and symbol are strings,and types is a string or character vector
  if (!is.list(tokens) || !is.character(encrypted_account_id) || !is.numeric(transaction_id)) {
    stop("Tokens must be a list, encrypted account ID must be a string, and transaction ID must be numeric.")
  }
  # Define base URL for GET request
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", encrypted_account_id, "/transactions/", transaction_id)
  # Define list to hold error messages
  error_messages <- list(
    "400" = "400 error - validation problem with the request. Double check input objects, including tokens, and try again.",
    "401" = "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application.",
    "403" = "403 error - caller is forbidden from accessing this service.",
    "404" = "404 error - resource is not found. Double check inputs and try again later.",
    "500" = "500 error - unexpected server error. Please try again later.",
    "503" = "503 error - server has a temporary problem responding. Please try again later."
  )
  # Send GET request
  request <- httr::GET(url = url,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token)))
  # Extract status code from request as string
  request_status_code <- as.character(httr::status_code(request))
  # Extract content from request
  req_list <- httr::content(request)
  # Check for valid response (200)
  if (request_status_code == 200) {
    # Transform list to data frame
    req_df <- as.data.frame(req_list)
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
