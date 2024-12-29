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
# Test 3: Bad API authentication call throws error
test_that("bad API call throws error", {
  expect_message(get_accounts(list(),
                            fields = "test"), # nolint
               "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application. More specifics on error are below:") # nolint
})
