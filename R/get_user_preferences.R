#' Get User Preferences
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function, returns the user preferences associated with the
#' account that was authenticated.
#'
#' @return Returns a data frame containing the user's preferences
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords accounts preferences
#' @importFrom httr GET add_headers content status_code
#' @importFrom stringr str_extract
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#'
get_user_preferences <- function(tokens) {
  # Ensure tokens parameter is a list
  if (!is.list(tokens)) {
    stop("Tokens parameter must be a list.")
  }
  # Define base URL for GET request
  url <- "https://api.schwabapi.com/trader/v1/userPreference"
  # Send GET request
  request <- httr::GET(url = url,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform list to data frame
    req_df <- as.data.frame(req_list)
    # Clean up names in data frame
    names(req_df) <- stringr::str_extract(names(req_df), "(?<=\\.)[^\\.]*$")
    # Return data frame
    return(req_df)
    # If invalid response returned, stop function and inform user
  } else {
    stop("Error during call - please check token input object and ensure access token is refreshed.") # nolint
  }
}
