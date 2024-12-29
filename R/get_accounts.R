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
  # Extract status code from request
  request_status_code <- httr::status_code(request)
  # Extract content from request
  req_list <- httr::content(request)
  # Verify that a proper response is returned (200)
  if (request_status_code == 200) {
    # Transform list to data frame
    req_df <- as.data.frame(req_list)
    # Clean up names in data frame
    names(req_df) <- stringr::str_extract(names(req_df), "(?<=\\.)[^\\.]*$")
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