# Unit tests for the replace_order function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(replace_order(1,
                             account_number = "test",
                             order_id = 1,
                             request_body = list()), # nolint
               "Tokens must be a list, account number must be a string, and the request body must be JSON.") # nolint
})
# Test 2: Invalid class type for account number throws error
test_that("invalid class type for account number throws error", {
  expect_error(replace_order(1,
                             account_number = 1,
                             order_id = 1,
                             request_body = list()), # nolint
               "Tokens must be a list, account number must be a string, and the request body must be JSON.") # nolint
})
# Test 3: Invalid class type for order ID throws error
test_that("invalid class type for order ID throws error", {
  expect_error(replace_order(1,
                             account_number = "test",
                             order_id = "error",
                             request_body = list()), # nolint
               "Tokens must be a list, account number must be a string, and the request body must be JSON.") # nolint
})
# Test 4: Bad API call throws error
test_that("bad API call throws error", {
  json_object <- list()
  class(json_object) <- "json"
  expect_error(replace_order(list(),
                             account_number = "test",
                             order_id = 1,
                             request_body = json_object), # nolint
               "Error during API call - please check inputs and ensure access token is refreshed.") # nolint
})
# Test 5: Invalid class type for request body throws error
test_that("invalid class type for request body throws error", {
  expect_error(replace_order(1,
                             account_number = "test",
                             order_id = 1,
                             request_body = FALSE), # nolint
               "Tokens must be a list, account number must be a string, and the request body must be JSON.") # nolint
})
