#' Get Account Transactions
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the encrypted ID for the account number of interest, get
#' the transactions associated with that account number. By
#' default, the last 180 days worth of transactions are returned.
#' However, this can be tweaked according to the date parameters
#' along with the types of transactions using the types parameter.
#'
#' @return Returns a data frame containing the account's transaction
#'         information.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords account transactions
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @importFrom lubridate days
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param encrypted_account_id encrypted ID of the account from `get_account_numbers` function (string).
#' @param start_datetime datetime that you would like to start gathering transactions from, in yyyy-mm-dd'T'HH:mm:ss.SSSZ format. Default is current datetime less 180 days (string).
#' @param end_datetime datetime that you would like to end gathering transactions from, in yyyy-mm-dd'T'HH:mm:ss.SSSZ format. Default is current datetime (string).
#' @param symbol filter for transactions based on this symbol. Default is NULL, which means no filtering (string).
#' @param types filter for transactions based on its type. Default is NULL, which means no filtering. Available values are 'TRADE', 'RECEIVE_AND_DELIVER', 'DIVIDEND_OR_INTEREST', 'ACH_RECEIPT', 'ACH_DISBURSEMENT', 'CASH_RECEIPT', 'CASH_DISBURSEMENT', 'ELECTRONIC_FUND', 'WIRE_OUT', 'WIRE_IN', 'JOURNAL', 'MEMORANDUM', 'MARGIN_CALL', 'MONEY_MARKET', or 'SMA_ADJUSTMENT' (string or character vector).
#'
get_transactions <- function(tokens,
                             encrypted_account_id,
                             start_datetime = strftime(Sys.time() - lubridate::days(180), format = "%Y-%m-%dT%H:%M:%OS3Z"), # nolint
                             end_datetime = strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3Z"), # nolint
                             symbol = NULL,
                             types = NULL) {
  # Ensure tokens parameter is a list, start/end dates and symbol are strings,and types is a string or character vector # nolint
  if (!is.list(tokens) || !is.character(encrypted_account_id) || !is.character(start_datetime) || !is.character(end_datetime) || (!is.null(symbol) && !is.character(symbol)) || (!is.null(types) && !is.character(types))) { # nolint
    stop("Tokens must be a list, start/end dates, symbol, and encrypted account ID must be strings, and types must be a string or character vector.") # nolint
  }
  # Ensure types is one of the appropriate values
  if (!is.null(types) && (length(setdiff(types, c("TRADE", "RECEIVE_AND_DELIVER", "DIVIDEND_OR_INTEREST", "ACH_RECEIPT", "ACH_DISBURSEMENT", "CASH_RECEIPT", "CASH_DISBURSEMENT", "ELECTRONIC_FUND", "WIRE_OUT", "WIRE_IN", "JOURNAL", "MEMORANDUM", "MARGIN_CALL", "MONEY_MARKET", "SMA_ADJUSTMENT")) > 0))) { # nolint
    stop("Types must be NULL or 'TRADE', 'RECEIVE_AND_DELIVER', 'DIVIDEND_OR_INTEREST', 'ACH_RECEIPT', 'ACH_DISBURSEMENT', 'CASH_RECEIPT', 'CASH_DISBURSEMENT', 'ELECTRONIC_FUND', 'WIRE_OUT', 'WIRE_IN', 'JOURNAL', 'MEMORANDUM', 'MARGIN_CALL', 'MONEY_MARKET', or 'SMA_ADJUSTMENT'.") # nolint
  }
  # Define base URL for GET request
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", encrypted_account_id, "/transactions") # nolint
  # Define payload
  query <- list("startDate" = start_datetime,
                "endDate" = end_datetime,
                "symbol" = symbol,
                "types" = types)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Extract status code from request
  request_status_code <- httr::status_code(request)
  # Extract content from request
  req_list <- httr::content(request)
  # Check if valid response returned (200)
  if (request_status_code == 200) {
    # Transform list to data frame
    req_df <- dplyr::bind_rows(req_list)
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
