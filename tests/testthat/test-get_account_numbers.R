# Unit tests for get_account_numbers function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_account_numbers(1),
               "Tokens parameter must be a list.") # nolint
})
# Test 2: Bad API call returns error
test_that("bad API call returns error", {
  expect_error(get_account_numbers(list()),
               "Error during API call - please check token input object and ensure access token is refreshed.") # nolint
})
