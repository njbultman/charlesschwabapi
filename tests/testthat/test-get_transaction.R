# Unit tests for the get_transaction function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_transaction(1),
               "Tokens must be a list, encrypted account ID must be a string, and transaction ID must be numeric.")
})
# Test 2: Invalid class type for transaction ID throws error
test_that("invalid value for transaction ID throws error", {
  expect_error(get_transaction(list(),
                               encrypted_account_id = "test",
                               transaction_id = "error"),
               "Tokens must be a list, encrypted account ID must be a string, and transaction ID must be numeric.")
})
# Test 3: Invalid class type for encrypted account ID throws error
test_that("invalid value for encrypted account ID throws error", {
  expect_error(get_transaction(list(),
                               encrypted_account_id = 1,
                               transaction_id = "test"),
               "Tokens must be a list, encrypted account ID must be a string, and transaction ID must be numeric.")
})
# Test 4: Bad API authentication call throws error
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_transaction(list(),
                                                encrypted_account_id = "test",
                                                 transaction_id = 1)),
                regexp = "Unauthorized")
})