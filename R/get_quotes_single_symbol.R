#' Get Quotes for Single Symbol
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the symbol ID, return a data frame
#' containing information about that symbol. Note that this
#' function can return information that goes beyond a standard
#' quote (for example, fundamental information can be returned).
#' By default, everything is returned, but the specific information
#' returned can be customized through the `fields` argument below.
#'
#' @return Returns a data frame containing information about the
#'         given symbol of interest.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords quote symbol_id
#' @importFrom httr GET add_headers content status_code
#' @importFrom stringr str_detect str_extract
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param symbol_id symbol for the security of interest
#' @param fields request for subset of data. Possible values are NULL, "all", "quote", "fundamental", "extended", "reference", or "regular". Note these can be combined in a vector or a string with a value can be used. The default is NULL, which is everything (string or vector).
#'
get_quotes_single_symbol <- function(tokens,
                                     symbol_id,
                                     fields = NULL) {
  # Ensure tokens parameter is a list and other parameters are strings or string vectors
  if (!is.list(tokens) || !is.character(symbol_id) || (!is.null(fields) && !is.character(fields))) { # nolint
    stop("Tokens parameter must be a list while symbol_id must be a string. Fields must be NULL, a string, or a character vector.") # nolint
  }
  # Values to check in fields vector
  fields_values <- c("all", "quote", "fundamental", "extended", "reference", "regular") # nolint
  # If vector contains all and others, throw error
  if (any(stringr::str_detect(fields, "all")) && length(fields) > 1) {
    stop("Fields parameter must be NULL, 'all' or combination of these: 'quote', 'fundamental', 'extended', 'reference', or 'regular'.") # nolint
    # If vector contains unsuitable values, throw error
  } else if ((!is.null(fields) && length(setdiff(fields, fields_values) > 0))) {
    stop("Fields parameter must be NULL, 'all' or combination of these: 'quote', 'fundamental', 'extended', 'reference', or 'regular'.") # nolint
  }
  # Define URL for GET request
  url <- paste0("https://api.schwabapi.com/marketdata/v1/",
                symbol_id,
                "/quotes")
  # Define query parameters
  query <- list("fields" = paste(fields, collapse = ","))
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform to data frame
    req_df <- as.data.frame(req_list)
    # Get original names
    names_req_df <- names(req_df)
    # Clean up names in data frame
    names(req_df) <- stringr::str_extract(names(req_df), "(?<=\\.)[^\\.]*$")
    # If name is NA after transformation, put in original name
    for (i in seq_len(length(names_req_df))) {
      if (is.na(names(req_df)[i])) {
        names(req_df)[i] <- names_req_df[i]
      }
    }
    # Return data frame
    return(req_df)
  } else {
    stop("Error during call - please check inputs and ensure access token is refreshed.") # nolint
  }
}
