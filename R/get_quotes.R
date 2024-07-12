#' Get Quotes
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the symbols of interest, return a data frame
#' containing information about those symbols. Note that this
#' function can return information that goes beyond a standard
#' quote (for example, fundamental information can be returned).
#' #' By default, everything is returned, but the specific information
#' returned can be customized through the `fields` argument below.
#'
#' @return Returns a data frame containing information about the
#'         given symbols of interest.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords quotes quote symbols
#' @importFrom httr GET add_headers content status_code
#' @importFrom stringr str_detect str_extract
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param symbols symbols for quotes (vector).
#' @param fields request for subset of data. Possible values are NULL, "all", "quote", "fundamental", "extended", "reference", or "regular". Note these can be combined in a vector or a string with a value can be used (string or vector).
#' @param indicative include indicative symbol quotes or not for ETF requests. Possible values are "true" and "false" (string).
#'
get_quotes <- function(tokens,
                       symbols,
                       fields = NULL,
                       indicative = NULL) {
  # Ensure tokens parameter is a list and other parameters are strings or string vectors # nolint
  if (!is.list(tokens) || !is.character(symbols) || (!is.null(fields) && !is.character(fields)) || (!is.null(indicative) && !is.character(indicative))) { # nolint
    stop("Tokens parameter must be a list, symbols and fields must be a string or character vector, and indicative must be a string.") # nolint
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
  url <- paste0("https://api.schwabapi.com/marketdata/v1/quotes")
  # Define query parameters
  query <- list("symbols" = paste(symbols, collapse = ","),
                "fields" = paste(fields, collapse = ","),
                "indicative" = indicative)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # If list is greater than one, bind together
    if (length(req_list) > 1) {
      # Transform list to data frames and bind rows
      req_df <- do.call(dplyr::bind_rows, lapply(req_list, as.data.frame))
    } else {
      # Transform to data frame
      req_df <- as.data.frame(req_list)
    }
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
    # If invalid response, stop and inform user
  } else {
    stop("Error during call - please check inputs and ensure access token is refreshed.") # nolint
  }
}
