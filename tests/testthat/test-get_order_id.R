# Unit tests for the get_order_id function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_order_id(1,
                            account_number = "test",
                            order_id = 1), # nolint
               "Tokens must be a list, account number must be a string, and the order ID must be numeric.") # nolint
})
# Test 2: Invalid class type for account number throws error
test_that("invalid class type for account number throws error", {
  expect_error(get_order_id(1,
                            account_number = 1,
                            order_id = 1), # nolint
               "Tokens must be a list, account number must be a string, and the order ID must be numeric.") # nolint
})
# Test 3: Invalid class type for order ID throws error
test_that("invalid class type for order ID throws error", {
  expect_error(get_order_id(1,
                            account_number = "test",
                            order_id = "error"), # nolint
               "Tokens must be a list, account number must be a string, and the order ID must be numeric.") # nolint
})
# Test 4: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_order_id(list(),
                            account_number = "test",
                            order_id = 1), # nolint
               "Error during API call - please check inputs and ensure access token is refreshed.") # nolint
})
