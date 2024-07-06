# Unit tests for get_accounts function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_accounts(1), # nolint
               "Tokens must be a list and fields must be NULL, a string, or character vector.") # nolint
})
# Test 2: Invalid class type for fields throws error
test_that("invalid class type for fields throws error", {
  expect_error(get_accounts(list(),
                            fields = 1), # nolint
               "Tokens must be a list and fields must be NULL, a string, or character vector.") # nolint
})
# Test 3: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_accounts(list(),
                            fields = "test"), # nolint
               "Error during API call - please check token input object, ensure access token is refreshed, and make sure fields is appropriately defined.") # nolint
})
