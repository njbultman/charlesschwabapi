# Unit tests for the get_transactions function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_transactions(1), # nolint
               "Tokens must be a list, start/end dates, symbol, and account number must be strings, and types must be a string or character vector.") # nolint
})
# Test 2: Invalid value for types throws error
test_that("invalid value for types throws error", {
  expect_error(get_transactions(list(),
                                account_number = "test",
                                types = "test"), # nolint
               "Types must be NULL or 'TRADE', 'RECEIVE_AND_DELIVER', 'DIVIDEND_OR_INTEREST', 'ACH_RECEIPT', 'ACH_DISBURSEMENT', 'CASH_RECEIPT', 'CASH_DISBURSEMENT', 'ELECTRONIC_FUND', 'WIRE_OUT', 'WIRE_IN', 'JOURNAL', 'MEMORANDUM', 'MARGIN_CALL', 'MONEY_MARKET', or 'SMA_ADJUSTMENT'.") # nolint
})
# Test 3: Invalid class type for account number throws error
test_that("invalid class type for account number throws error", {
  expect_error(get_transactions(list(),
                                account_number = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and account number must be strings, and types must be a string or character vector.") # nolint
})
# Test 4: Invalid class type for start datetime throws error
test_that("invalid class type for start datetime throws error", {
  expect_error(get_transactions(list(),
                                account_number = "test",
                                start_datetime = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and account number must be strings, and types must be a string or character vector.") # nolint
})
# Test 5: Invalid class type for end datetime throws error
test_that("invalid class type for end datetime throws error", {
  expect_error(get_transactions(list(),
                                account_number = "test",
                                end_datetime = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and account number must be strings, and types must be a string or character vector.") # nolint
})
# Test 6: Invalid class type for symbol throws error
test_that("invalid class type for symbol throws error", {
  expect_error(get_transactions(list(),
                                account_number = "test",
                                symbol = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and account number must be strings, and types must be a string or character vector.") # nolint
})
# Test 7: Invalid class type for types throws error
test_that("invalid class type for types throws error", {
  expect_error(get_transactions(list(),
                                account_number = "test",
                                types = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and account number must be strings, and types must be a string or character vector.") # nolint
})
# Test 8: Bad API call throws error
test_that("bad API throws error", {
  expect_error(get_transactions(list(),
                                account_number = "test"), # nolint
               "Error during API call - please check inputs and ensure access token is refreshed.") # nolint
})
