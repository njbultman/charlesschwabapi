#' Get Account Numbers
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, return the account number(s) information, including
#' the actual account number(s) and the encrypted ID(s) of the account.
#' The latter is used in other functions like orders and transactions
#' that require a specific encrypted account ID to be specified.
#'
#' @return Returns a data frame containing the account numbers
#'         and their encrypted values.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords account numbers encrypted
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#'
get_account_numbers <- function(tokens) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens)) {
    stop("Tokens parameter must be a list.")
  }
  # Define URL for GET request
  url <- "https://api.schwabapi.com/trader/v1/accounts/accountNumbers"
  # Send GET request
  request <- httr::GET(url = url,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if API call returned a good status code
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform list to data frame
    req_df <- dplyr::bind_rows(req_list)
    # Return data frame
    return(req_df)
  } else {
    stop("Error during API call - please check token input object and ensure access token is refreshed.") # nolint
  }
}
