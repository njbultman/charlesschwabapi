# Unit tests for the get_transactions function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_transactions(1), # nolint
               "Tokens must be a list, start/end dates, symbol, and encrypted account ID must be strings, and types must be a string or character vector.") # nolint
})
# Test 2: Invalid value for types throws error
test_that("invalid value for types throws error", {
  expect_error(get_transactions(list(),
                                encrypted_account_id = "test",
                                types = "test"), # nolint
               "Types must be NULL or 'TRADE', 'RECEIVE_AND_DELIVER', 'DIVIDEND_OR_INTEREST', 'ACH_RECEIPT', 'ACH_DISBURSEMENT', 'CASH_RECEIPT', 'CASH_DISBURSEMENT', 'ELECTRONIC_FUND', 'WIRE_OUT', 'WIRE_IN', 'JOURNAL', 'MEMORANDUM', 'MARGIN_CALL', 'MONEY_MARKET', or 'SMA_ADJUSTMENT'.") # nolint
})
# Test 3: Invalid class type for encrypted account ID throws error
test_that("invalid class type for encrypted account ID throws error", {
  expect_error(get_transactions(list(),
                                encrypted_account_id = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and encrypted account ID must be strings, and types must be a string or character vector.") # nolint
})
# Test 4: Invalid class type for start datetime throws error
test_that("invalid class type for start datetime throws error", {
  expect_error(get_transactions(list(),
                                encrypted_account_id = "test",
                                start_datetime = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and encrypted account ID must be strings, and types must be a string or character vector.") # nolint
})
# Test 5: Invalid class type for end datetime throws error
test_that("invalid class type for end datetime throws error", {
  expect_error(get_transactions(list(),
                                encrypted_account_id = "test",
                                end_datetime = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and encrypted account ID must be strings, and types must be a string or character vector.") # nolint
})
# Test 6: Invalid class type for symbol throws error
test_that("invalid class type for symbol throws error", {
  expect_error(get_transactions(list(),
                                encrypted_account_id = "test",
                                symbol = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and encrypted account ID must be strings, and types must be a string or character vector.") # nolint
})
# Test 7: Invalid class type for types throws error
test_that("invalid class type for types throws error", {
  expect_error(get_transactions(list(),
                                encrypted_account_id = "test",
                                types = 1), # nolint
               "Tokens must be a list, start/end dates, symbol, and encrypted account ID must be strings, and types must be a string or character vector.") # nolint
})
# Test 8: Bad API authentication call throws error
test_that("bad API call throws error", {
  expect_message(get_transactions(list(),
                                  encrypted_account_id = "test"),
               "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application. More specifics on error are below:") # nolint
})
