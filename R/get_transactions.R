#' Get Account Transactions
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the encrypted account number of interest, get
#' the transactions associated with that account number. By
#' default, the last year's worth of transactions are returned.
#' However, this can be tweaked according to the date parameters
#' along with the types of transactions using the types parameter.
#'
#' @return Returns a data frame containing the account's transaction
#'         information.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords account transactions
#' @importFrom httr GET add_headers content status_code
#' @importFrom dplyr bind_rows
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param account_number encrypted ID of the account (string).
#' @param start_datetime datetime that you would like to start gathering transactions from, in yyyy-mm-dd'T'HH:mm:ss.SSSZ format (string).
#' @param end_datetime datetime that you would like to end gathering transactions from, in yyyy-mm-dd'T'HH:mm:ss.SSSZ format (string).
#' @param symbol filter for transactions based on this symbol. Defaults is NULL, which means no filtering (string).
#' @param types filter for transactions based on its type. Defaults is NULL, which means no filtering. Available values are 'TRADE', 'RECEIVE_AND_DELIVER', 'DIVIDEND_OR_INTEREST', 'ACH_RECEIPT', 'ACH_DISBURSEMENT', 'CASH_RECEIPT', 'CASH_DISBURSEMENT', 'ELECTRONIC_FUND', 'WIRE_OUT', 'WIRE_IN', 'JOURNAL', 'MEMORANDUM', 'MARGIN_CALL', 'MONEY_MARKET', or 'SMA_ADJUSTMENT' (string or character vector).
#'
get_transactions <- function(tokens,
                             account_number,
                             start_datetime = strftime(Sys.time() - lubridate::years(1), format = "%Y-%m-%dT%H:%M:%OS3Z"), # nolint
                             end_datetime = strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%OS3Z"), # nolint
                             symbol = NULL,
                             types = NULL) {
  # Ensure tokens parameter is a list, start/end dates and symbol are strings,and types is a string or character vector # nolint
  if (!is.list(tokens) || !is.character(account_number) || !is.character(start_datetime) || !is.character(end_datetime) || (!is.null(symbol) && !is.character(symbol)) || (!is.null(types) && !is.character(types))) { # nolint
    stop("Tokens must be a list, start/end dates, symbol, and account number must be strings, and types must be a string or character vector.") # nolint
  }
  # Ensure types is one of the appropriate values
  if (!is.null(types) && (length(setdiff(types, c("TRADE", "RECEIVE_AND_DELIVER", "DIVIDEND_OR_INTEREST", "ACH_RECEIPT", "ACH_DISBURSEMENT", "CASH_RECEIPT", "CASH_DISBURSEMENT", "ELECTRONIC_FUND", "WIRE_OUT", "WIRE_IN", "JOURNAL", "MEMORANDUM", "MARGIN_CALL", "MONEY_MARKET", "SMA_ADJUSTMENT")) > 0))) { # nolint
    stop("Types must be NULL or 'TRADE', 'RECEIVE_AND_DELIVER', 'DIVIDEND_OR_INTEREST', 'ACH_RECEIPT', 'ACH_DISBURSEMENT', 'CASH_RECEIPT', 'CASH_DISBURSEMENT', 'ELECTRONIC_FUND', 'WIRE_OUT', 'WIRE_IN', 'JOURNAL', 'MEMORANDUM', 'MARGIN_CALL', 'MONEY_MARKET', or 'SMA_ADJUSTMENT'.") # nolint
  }
  # Define base URL for GET request
  url <- paste0("https://api.schwabapi.com/trader/v1/accounts/", account_number, "/transactions") # nolint
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
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Extract content from request
    req_list <- httr::content(request)
    # Transform list to data frame
    req_df <- dplyr::bind_rows(req_list)
    # Return data frame
    return(req_df)
    # If invalid response, stop program and inform user
  } else {
    stop("Error during API call - please check inputs and ensure access token is refreshed.") # nolint
  }
}
