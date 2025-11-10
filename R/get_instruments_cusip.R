#' Get Instruments by Cusip
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and a cusip, return a data frame with information
#' about the security matching the search.
#'
#' @return Returns a data frame containing information surrounding
#'         the cusip of interest in the search.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords instrument instruments search cusip
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens tokens object from `get_authentication_tokens` function (list).
#' @param cusip_id cusip of the security to be searched on (string).
#'
get_instruments_cusip <- function(tokens, cusip_id) {
  # Ensure tokens parameter is a listand cusip_id is a string
  if (!is.list(tokens) || !is.character(cusip_id)) {
    stop("Tokens parameter must be a list and cusip must be a string.")
  }
  # Define URL for GET request
  url <- paste0("https://api.schwabapi.com/marketdata/v1/instruments/", cusip_id)
  # Define list to hold error messages
  error_messages <- list(
    "400" = "400 error - validation problem with the request. Double check input objects, including tokens, and try again.",
    "401" = "401 error - authorization token is invalid.",
    "404" = "404 error - resource is not found. Double check inputs and try again later.",
    "500" = "500 error - unexpected server error. Please try again later."
  )
  # Send GET request
  request <- httr::GET(url = url,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token)))
  # Extract status code from request as string
  request_status_code <- as.character(httr::status_code(request))
  # If content can be extracted, check if valid response returned (200)
  if (request_status_code == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Bind all objects from list to a data frame
    req_df <- dplyr::bind_rows(req_list)
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
    print(unlist(request))
  }
}
