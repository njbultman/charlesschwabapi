# Unit tests for the get_transaction function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_transaction(1), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and transaction ID must be numeric.") # nolint
})
# Test 2: Invalid class type for transaction ID throws error
test_that("invalid value for transaction ID throws error", {
  expect_error(get_transaction(list(),
                               encrypted_account_id = "test",
                               transaction_id = "error"), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and transaction ID must be numeric.") # nolint
})
# Test 3: Invalid class type for encrypted account ID throws error
test_that("invalid value for encrypted account ID throws error", {
  expect_error(get_transaction(list(),
                               encrypted_account_id = 1,
                               transaction_id = "test"), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and transaction ID must be numeric.") # nolint
})
# Test 4: Bad API authentication call throws error
test_that("bad API call throws error", {
  expect_message(get_transaction(list(),
                                 encrypted_account_id = "test",
                                 transaction_id = 1),
               "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application. More specifics on error are below:") # nolint
})