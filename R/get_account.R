#' Get Account Information for Specific Account
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the encrypted account ID, return the account
#' information (positions, fundamentals, and general account
#' information). The encrypted account ID can be found using the
#' `get_account_numbers` function.
#'
#' @return Returns a data frame containing the account
#'         information. This includes position information
#'         a day trader flag, the account number, and more.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords account number positions
#' @importFrom httr GET add_headers content status_code
#' @importFrom stringr str_extract
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param account_number encrypted ID of the account (string).
#' @param fields specific fields to be returned, one example being "positions" (string).
#'
get_account <- function(tokens,
                        account_number,
                        fields = NULL) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(account_number) || (!is.null(fields) && !is.character(fields))) { # nolint
    stop("Tokens must be a list, account number must be a string, and fields must be NULL, a string, or character vector.") # nolint
  }
  # Define URL for GET request
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", account_number)
  # Define payload
  query <- list("fields" = fields)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if request returned a proper response (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform list to data frame
    req_df <- as.data.frame(req_list)
    # Clean up names in data frame
    names(req_df) <- stringr::str_extract(names(req_df), "(?<=\\.)[^\\.]*$")
    # Return data frame
    return(req_df)
    # If a bad response is returned, halt program and inform user
  } else {
    stop("Error during API call - please check token input object and ensure access token is refreshed in addition to other arguments being specified correctly.") # nolint
  }
}
