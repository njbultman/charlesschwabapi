#' Get Account Numbers
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, return the account number(s) information, including
#' the actual account number(s) and the encrypted ID(s) of the user
#' that were granted when authenticating. The encrypted IDs are used
#' in other functions (like orders and transactions) that require a
#' specific encrypted account ID to be specified.
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
  # Check if API call returned a good status code
  if (request_status_code == 200) {
    # return transformed list into data frame
    return(dplyr::bind_rows(req_list))
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
