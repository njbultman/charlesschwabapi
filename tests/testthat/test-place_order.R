# Unit tests for the place_order function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(place_order(1,
                           encrypted_account_id = "test",
                           request_body = list()),
               "Tokens must be a list, encrypted account ID must be a string, and the request body must be JSON.")
})
# Test 2: Invalid class type for encrypted account ID throws error
test_that("invalid class type for encrypted account ID throws error", {
  expect_error(place_order(1,
                           encrypted_account_id = 1,
                           request_body = list()),
               "Tokens must be a list, encrypted account ID must be a string, and the request body must be JSON.")
})
# Test 3: Invalid class type for request body throws error
test_that("invalid class type for request body throws error", {
  expect_error(place_order(1,
                           encrypted_account_id = "test",
                           request_body = "error"),
               "Tokens must be a list, encrypted account ID must be a string, and the request body must be JSON.")
})
# Test 4: Bad API authentication call throws error
test_that("bad API call returns error", {
  json_object <- list()
  class(json_object) <- "json"
  expect_output(suppressMessages(place_order(list(),
                                             encrypted_account_id = "test",
                                             request_body = json_object)),
                regexp = "Unauthorized")
})
