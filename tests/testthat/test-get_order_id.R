# Unit tests for the get_order_id function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_order_id(1,
                            encrypted_account_id = "test",
                            order_id = 1),
               "Tokens must be a list, encrypted account ID must be a string, and the order ID must be numeric.")
})
# Test 2: Invalid class type for encrypted account ID throws error
test_that("invalid class type for encrypted account ID throws error", {
  expect_error(get_order_id(1,
                            encrypted_account_id = 1,
                            order_id = 1),
               "Tokens must be a list, encrypted account ID must be a string, and the order ID must be numeric.")
})
# Test 3: Invalid class type for order ID throws error
test_that("invalid class type for order ID throws error", {
  expect_error(get_order_id(1,
                            encrypted_account_id = "test",
                            order_id = "error"),
               "Tokens must be a list, encrypted account ID must be a string, and the order ID must be numeric.")
})
# Test 4: Bad API authentication call throws error
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_order_id(list(),
                                              encrypted_account_id = "test",
                                              order_id = 1)),
                regexp = "Unauthorized")
})
