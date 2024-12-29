# Unit tests for the cancel_order function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(cancel_order(1,
                            encrypted_account_id = "test",
                            order_id = 1), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and the order ID must be numeric.") # nolint
})
# Test 2: Invalid class type for account number throws error
test_that("invalid class type for account number throws error", {
  expect_error(cancel_order(1,
                            encrypted_account_id = 1,
                            order_id = 1), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and the order ID must be numeric.") # nolint
})
# Test 3: Invalid class type for order ID throws error
test_that("invalid class type for order ID throws error", {
  expect_error(cancel_order(1,
                            encrypted_account_id = "test",
                            order_id = "error"), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and the order ID must be numeric.") # nolint
})
# Test 4: Bad API call for improper authentication tokens throws a 401 error message
test_that("bad API call throws message", {
  expect_message(cancel_order(list(),
                              encrypted_account_id = "test",
                              order_id = 1),
                  "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application. More specifics on error are below:") # nolint
})