#' Get Accounts Information
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, return the account(s) information associated with
#' the authenticated user. By default, this includes positions,
#' fundamentals, and general account information. However, one
#' can use the `fields` argument to get more specific as to the
#' information returned.
#'
#' @return Returns a data frame containing the account(s)
#'         information.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords accounts positions
#' @importFrom httr GET add_headers content status_code
#' @importFrom stringr str_extract
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param fields specific fields to be returned, an example being "positions" (string).
#'
get_accounts <- function(tokens,
                         fields = NULL) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || (!is.null(fields) && !is.character(fields))) { # nolint
    stop("Tokens must be a list and fields must be NULL, a string, or character vector.") # nolint
  }
  # Define base URL for GET request
  url <- "https://api.schwabapi.com/trader/v1/accounts"
  # Define payload
  query <- list("fields" = paste0(fields, collapse = ","))
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Verify that a proper response is returned (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform list to data frame
    req_df <- as.data.frame(req_list)
    # Clean up names in data frame
    names(req_df) <- stringr::str_extract(names(req_df), "(?<=\\.)[^\\.]*$")
    # Return data frame
    return(req_df)
    # If proper response is not returned, throw error and inform user
  } else {
    stop("Error during API call - please check token input object, ensure access token is refreshed, and make sure fields is appropriately defined.") # nolint
  }
}