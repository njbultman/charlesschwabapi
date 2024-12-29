# Unit tests for the replace_order function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(replace_order(1,
                             encrypted_account_id = "test",
                             order_id = 1,
                             request_body = list()), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and the request body must be JSON.") # nolint
})
# Test 2: Invalid class type for encrypted account ID throws error
test_that("invalid class type for encrypted account ID throws error", {
  expect_error(replace_order(1,
                             encrypted_account_id = 1,
                             order_id = 1,
                             request_body = list()), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and the request body must be JSON.") # nolint
})
# Test 3: Invalid class type for order ID throws error
test_that("invalid class type for order ID throws error", {
  expect_error(replace_order(1,
                             encrypted_account_id = "test",
                             order_id = "error",
                             request_body = list()), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and the request body must be JSON.") # nolint
})
# Test 4: Bad API authentication call throws error
test_that("bad API call throws error", {
  json_object <- list()
  class(json_object) <- "json"
  expect_message(replace_order(list(),
                               encrypted_account_id = "test",
                               order_id = 1,
                               request_body = json_object), # nolint
               "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application. More specifics on error are below:") # nolint
})
# Test 5: Invalid class type for request body throws error
test_that("invalid class type for request body throws error", {
  expect_error(replace_order(1,
                             encrypted_account_id = "test",
                             order_id = 1,
                             request_body = FALSE), # nolint
               "Tokens must be a list, encrypted account ID must be a string, and the request body must be JSON.") # nolint
})
