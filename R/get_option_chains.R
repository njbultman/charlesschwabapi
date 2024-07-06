#' Get Option Chains
#'
#' Given the tokens object from the `get_authentication_tokens`
#' function and the symbol of interest, return a data frame
#' containing information about the option chain. Note that there
#' are many ways to customize the data returned, including being
#' able to specify the strategy as analytical and providing your
#' own input assumptions. Refer to the parameters below for more
#' detailed information.
#'
#' @return Returns a data frame containing information about the
#'         given symbol's option chain.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, June 2024
#' @keywords option chains
#' @importFrom httr GET add_headers content status_code
#' @importFrom stringr str_replace
#' @importFrom lubridate is.Date
#' @importFrom dplyr bind_rows mutate
#' @importFrom tidyr pivot_longer
#' @importFrom purrr keep
#' @export
#'
#' @param tokens token object from `get_authentication_tokens` function (list).
#' @param symbol symbol of interest (string).
#' @param contract_type string containing "ALL", "PUT", or "CALL". Default is NULL, which is "ALL" (string).
#' @param strike_count number of strikes to return above or below the at-the-monty price (numeric). Default is NULL, which will return all.
#' @param include_underlying_quote flag to indicate whether underlying quotes to be included. Default is NULL, which is TRUE (boolean).
#' @param strategy type of strategy to return, can be "SINGLE", "ANALYTICAL", "COVERED", "VERTICAL", "CALENDAR", "STRANGLE", "STRADDLE", "BUTTERFLY", "CONDOR", "DIAGONAL", "COLLAR", or "ROLL". Default is NULL, which is "SINGLE" (string).
#' @param interval strike interval for the spread strategy chains (see strategy parameter). Default is NULL (numeric).
#' @param strike strike price - default is NULL (which will not return a specific strike price) (numeric).
#' @param range range (ITM, NTM, OTM, etc.). Default is NULL, which will return all (string).
#' @param from_date from date - default is NULL, which will return all (date).
#' @param to_date to date - default is NULL, which will return all (date).
#' @param volatility volatility to use in calculation and only applies to ANALYTICAL strategy (see strategy parameter). Default is NULL. (numeric).
#' @param underlying_price underlying price to use in calculation and only applies to ANALYTICAL strategy (see strategy parameter). Default is NULL (numeric).
#' @param interest_rate interest rate to use in calculation and only applies to ANALYTICAL strategy (see strategy parameter). Default is NULL (numeric).
#' @param days_to_expiration days to expiration to use in calculation and only applies to ANALYTICAL strategy (see strategy parameter). Default is NULL (numeric).
#' @param exp_month expiration month - valid values are "ALL", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", or "DEC". Default is NULL, which is "ALL" (string).
#' @param option_type option type - default is NULL, which will return everything (string).
#' @param entitlement applicable only for retail token, entitlement of client PP-PayingPro, NP-NonPro, and PN-NonPayingPro. Valid values are "PN", "NP", "PP" - default is NULL, which will return everything (string).
#'
get_option_chains <- function(tokens,
                              symbol,
                              contract_type = NULL,
                              strike_count = NULL,
                              include_underlying_quote = NULL,
                              strategy = NULL,
                              interval = NULL,
                              strike = NULL,
                              range = NULL,
                              from_date = NULL,
                              to_date = NULL,
                              volatility = NULL,
                              underlying_price = NULL,
                              interest_rate = NULL,
                              days_to_expiration = NULL,
                              exp_month = NULL,
                              option_type = NULL,
                              entitlement = NULL) {
  # Ensure tokens is a list and strike_count, interval, and strike are numeric while range is a string #nolint
  if (!is.list(tokens) || (!is.null(strike_count) && !is.numeric(strike_count)) || (!is.null(interval) && !is.numeric(interval)) || (!is.null(strike) && !is.numeric(strike)) || (!is.null(range) && !is.character(range)) || (!is.null(option_type) && !is.character(option_type))) { # nolint
    stop("Tokens must be a list and strike count, interval, and strike must be NULL or numeric, and range & option type must be NULL or character.") # nolint
  }
  # Ensure to and from dates are dates or NULLs
  if ((!is.null(to_date) && !lubridate::is.Date(to_date)) || (!is.null(from_date) && !lubridate::is.Date(from_date))) { # nolint
    stop("To/from dates must be dates or NULL.")
  }
  # Ensure strategy is NULL or "SINGLE", "ANALYTICAL", "COVERED", "VERTICAL", "CALENDAR", "STRANGLE", "STRADDLE", "BUTTERFLY", "CONDOR", "DIAGONAL", "COLLAR", "ROLL" # nolint
  if (!is.null(strategy) &&(length(setdiff(strategy, c("SINGLE", "ANALYTICAL", "COVERED", "VERTICAL", "CALENDAR", "STRANGLE", "STRADDLE", "BUTTERFLY", "CONDOR", "DIAGONAL", "COLLAR", "ROLL")) > 0))) { # nolint
    stop("Strategy must be NULL or 'SINGLE', 'ANALYTICAL', 'COVERED', 'VERTICAL', 'CALENDAR', 'STRANGLE', 'STRADDLE', 'BUTTERFLY', 'CONDOR', 'DIAGONAL', 'COLLAR', or 'ROLL'.") # nolint
  }
  # Ensure expiration month is NULL or "ALL", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" # nolint
  if (!is.null(exp_month) &&(length(setdiff(exp_month, c("ALL", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")) > 0))) { # nolint
    stop("Expiration month must be NULL or 'ALL', 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', or 'DEC'.") # nolint
  }
  # Ensure contract type is NULL or "CALL", "PUT", or "ALL"
  if (!is.null(contract_type) && (length(setdiff(contract_type, c("ALL", "CALL", "PUT")) > 0))) { # nolint
    stop("Contract type must be NULL or 'ALL', 'CALL', or 'PUT'.") # nolint
  }
  # Ensure entitlement is NULL or "PP", "NP", or "PN"
  if (!is.null(entitlement) && (length(setdiff(entitlement, c("PP", "NP", "PN")) > 0))) { # nolint
    stop("Entitlement must be NULL or 'PP', 'NP', or 'PN'.") # nolint
  }
  # Ensure include_underlying_quote is TRUE or FALSE
  if (!is.null(include_underlying_quote) && !is.logical(include_underlying_quote)) { # nolint
    stop("Include underlying quote must be NULL or logical (TRUE or FALSE).") # nolint
  }
    # Ensure volatility, underlying_price, interest_rate, and days_to_expiration are not NULL only when strategy is ANALYTICAL # nolint
  if (strategy != "ANALYTICAL" && (!is.null(volatility) || !is.null(underlying_price) || !is.null(interest_rate) || !is.null(days_to_expiration))) { # nolint
    stop("Volatility, underlying price, interest rate, and days to expiration should only be non-NULL when strategy is ANALYTICAL.") # nolint
  }
    # Ensure volatility, underlying price, interest rate, and days to expiration are numeric or NULL # nolint
  if ((!is.null(volatility) && !is.numeric(volatility)) || (!is.null(underlying_price) && !is.numeric(underlying_price)) || (!is.null(interest_rate) && !is.numeric(interest_rate)) || (!is.null(days_to_expiration) && !is.numeric(days_to_expiration))) { # nolint
    stop("Volatility, underlying price, interest rate, and days to expiration should be NULL or numeric (and non-NULL when strategy is ANALYTICAL).") # nolint
  }
  # Define URL for GET request
  url <- paste0("https://api.schwabapi.com/marketdata/v1/chains")
  # Define query parameters
  query <- list("symbol" = symbol,
                "contractType" = contract_type,
                "strikeCount" = strike_count,
                "includeUnderlyingQuote" = ifelse(include_underlying_quote, TRUE, FALSE), # nolint
                "strategy" = strategy,
                "interval" = interval,
                "strike" = strike,
                "range" = range,
                "fromDate" = strftime(from_date, "%y-%m-%d"),
                "toDate" = strftime(to_date, "%y-%m-%d"),
                "volatility" = volatility,
                "underlyingPrice" = underlying_price,
                "interestRate" = interest_rate,
                "daysToExpiration" = days_to_expiration,
                "expMonth" = exp_month,
                "optionType" = option_type,
                "entitlement" = entitlement)
  # Send GET request
  request <- httr::GET(url = url,
                       query = query,
                       httr::add_headers(`accept` = "application/json",
                                         `Authorization` = paste0("Bearer ", tokens$access_token))) # nolint
  # Check if valid response returned (200)
  if (httr::status_code(request) == 200) {
    # Inform user call was successful, so starting process
    print(paste0("Call successful for ", symbol, ", preparing data now, which can take some time depending on the symbol...")) # nolint
    # Extract content from request
    req_list <- httr::content(request)
    # Only keep elements that have one value (these will be appended to final data frame later) # nolint
    req_list_subset <- purrr::keep(req_list, function(x) length(x) == 1)
    # Transform these elements into their own data frame
    req_list_subset_df <- data.frame(req_list_subset)
    # Check i strategy is SINGLE or ANALYTICAL
    if (strategy == "SINGLE" || strategy == "ANALYTICAL" || is.null(strategy)) {
      # Bind contents of call into starting data frame
      req_df <- do.call(dplyr::bind_rows, lapply(append(req_list$callExpDateMap, req_list$putExpDateMap), as.data.frame)) # nolint
      # Only keep the last part of name after second period (first part is the strike) # nolint
      req_df_names <- sub("(?:[^\\.]*\\.){2}(.*?)", "", names(req_df))
      # Group similar columns together based on names
      req_df_list <- sapply(req_df_names,
                            function(x) req_df[,sub("(?:[^\\.]*\\.){2}(.*?)", "", names(req_df)) == x], # nolint
                            simplify = FALSE)
      # Melt each data frame to long format
      req_df_list_long <- lapply(req_df_list,
                                 function(x) tidyr::pivot_longer(x, # nolint
                                                                 cols = names(x), # nolint
                                                                 names_to = "strike", # nolint
                                                                 values_to = unique(sub("(?:[^\\.]*\\.){2}(.*?)", "", names(x))))) # nolint
      # Clean up strike values in each data frame so they are the same
      req_df_list_long_strike_clean <- lapply(req_df_list_long, function(x) dplyr::mutate(x, strike = stringr::str_replace(strike, paste0(".", names(x)[2]), ""))) # nolint
      # Keep first strike column and then drop others
      strike_vals <- req_df_list_long_strike_clean[[1]]$strike
      req_df_list_long_strike_clean <- lapply(req_df_list_long_strike_clean, function(x) dplyr::mutate(x, strike = NULL)) # nolint
      # Bind values to one data frame within each list component
      req_df_list_long_strike_clean_bind <- do.call(cbind.data.frame, req_df_list_long_strike_clean) # nolint
      # Add strike values back
      req_df_list_long_strike_clean_bind$strike <- strike_vals # nolint
      # Remove duplicated columns
      req_df_list_long_strike_clean_bind <- req_df_list_long_strike_clean_bind[, !duplicated(colnames(req_df_list_long_strike_clean_bind))] # nolint
      # Remove duplicated values
      req_df_full <- req_df_list_long_strike_clean_bind[!duplicated(req_df_list_long_strike_clean_bind), ] # nolint
      # Add columns that only contain one value (subsetted in the beginning)
      for (i in names(req_list_subset_df)) {
        req_df_full[paste0(i)] <- req_list_subset_df[paste0(i)]
      }
    } else {
      # Bind contents of call into starting data frame
      req_df_full <- do.call(dplyr::bind_rows, lapply(req_list$monthlyStrategyList, as.data.frame)) # nolint
      # Add columns that only contain one value (subsetted in the beginning)
      for (i in names(req_list_subset_df)) {
        req_df_full[paste0(i)] <- req_list_subset_df[paste0(i)]
      }
    }
    # Return data frame
    return(req_df_full)
    # If invalid response returned, throw error and inform user
  } else {
    stop("Error during API call - please check inputs and ensure access token is refreshed. Also, be sure to check that the symbol is specified correctly.") # nolint
  }
}
