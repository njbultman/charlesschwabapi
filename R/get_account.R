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
#' @param encrypted_account_id encrypted ID of the account from `get_account_numbers` function (string).
#' @param fields specific fields to be returned, one example being "positions" (string).
#'
get_account <- function(tokens,
                        encrypted_account_id,
                        fields = NULL) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens) || !is.character(encrypted_account_id) || (!is.null(fields) && !is.character(fields))) { # nolint
    stop("Tokens must be a list, encrypted account ID must be a string, and fields must be NULL, a string, or character vector.") # nolint
  }
  # Define URL for GET request
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", encrypted_account_id) # nolint
  # Define payload
  query <- list("fields" = fields)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Extract status code from request
  request_status_code <- httr::status_code(request)
  # Extract content from request
  req_list <- httr::content(request)
  # Check if request returned a proper response (200)
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
