# Unit tests for get_account function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_account(1,
                           account_number = "test"),
               "Tokens must be a list, account number must be a string, and fields must be NULL, a string, or character vector.") # nolint
})
# Test 2: Invalid class type for fields throws error
test_that("invalid class type for fields throws error", {
  expect_error(get_account(list(),
                           account_number = "test",
                           fields = 1),
               "Tokens must be a list, account number must be a string, and fields must be NULL, a string, or character vector.") # nolint
})
# Test 3: Invalid class type for account_number throws error
test_that("invalid class type for account_number throws error", {
  expect_error(get_account(list(),
                           account_number = "error",
                           fields = 1),
               "Tokens must be a list, account number must be a string, and fields must be NULL, a string, or character vector.") # nolint
})
# Test 4: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_account(list(),
                           account_number = "test",
                           fields = "test"),
               "Error during API call - please check token input object and ensure access token is refreshed in addition to other arguments being specified correctly.") # nolint
})
