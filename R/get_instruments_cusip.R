#' Get Instruments by Cusip
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and a cusip, return a data frame with information
#' about the security matching the search.
#'
#' @return Returns a data frame containing information surrounding
#'         the symbol(s) of interest in the search.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, July 2024
#' @keywords instrument instruments search
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
  url <- paste0("https://api.schwabapi.com/marketdata/v1/instruments/", cusip_id) # nolint
  # Send GET request
  request <- httr::GET(url = url,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Bind all objects from list to a data frame
    req_df <- dplyr::bind_rows(req_list)
    # Return data frame
    return(req_df)
    # If invalid response, halt function and inform user
  } else {
    stop("Error during API call - please check cusip & token input object, and ensure access token is refreshed.") # nolint
  }
}