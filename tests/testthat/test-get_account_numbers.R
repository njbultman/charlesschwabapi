# Unit tests for get_account_numbers function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_account_numbers(1),
               "Tokens parameter must be a list.") # nolint
})
# Test 2: Bad API authentication call returns message
test_that("bad API call returns error", {
  expect_message(get_account_numbers(list()),
               "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application. More specifics on error are below:") # nolint
})
