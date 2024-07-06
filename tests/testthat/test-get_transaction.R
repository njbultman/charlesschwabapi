# Unit tests for the get_transaction function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_transaction(1), # nolint
               "Tokens must be a list, account number must be a string, and transaction ID must be numeric.") # nolint
})
# Test 2: Invalid class type for transaction ID throws error
test_that("invalid value for transaction ID throws error", {
  expect_error(get_transaction(list(),
                               account_number = "test",
                               transaction_id = "error"), # nolint
               "Tokens must be a list, account number must be a string, and transaction ID must be numeric.") # nolint
})
# Test 3: Invalid class type for account number throws error
test_that("invalid value for account number throws error", {
  expect_error(get_transaction(list(),
                               account_number = 1,
                               transaction_id = "test"), # nolint
               "Tokens must be a list, account number must be a string, and transaction ID must be numeric.") # nolint
})
# Test 4: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_transaction(list(),
                               account_number = "test",
                               transaction_id = 1), # nolint
               "Error during API call - please inputs and ensure access token is refreshed.") # nolint
})
