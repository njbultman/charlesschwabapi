#' Get Specific Transaction Information
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, the encrypted account number of interest, and the
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
#' @param account_number encrypted ID of the account (string).
#' @param transaction_id transaction ID of interest (numeric).
#'
get_transaction <- function(tokens,
                            account_number,
                            transaction_id) {
  # Ensure tokens parameter is a list, start/end dates and symbol are strings,and types is a string or character vector # nolint
  if (!is.list(tokens) || !is.character(account_number) || !is.numeric(transaction_id)) { # nolint
    stop("Tokens must be a list, account number must be a string, and transaction ID must be numeric.") # nolint
  }
  # Define base URL for GET request
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", account_number, "/transactions/", transaction_id) # nolint
  # Send GET request
  request <- httr::GET(url = url,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check for valid response (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform list to data frame
    req_df <- as.data.frame(req_list)
    # Return data frame
    return(req_df)
    # If invalid response, throw error and inform user
  } else {
    stop("Error during API call - please inputs and ensure access token is refreshed.") # nolint
  }
}
