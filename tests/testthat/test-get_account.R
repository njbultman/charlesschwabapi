# Unit tests for get_account function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_account(1,
                           encrypted_account_id = "test"),
               "Tokens must be a list, encrypted account ID must be a string, and fields must be NULL, a string, or character vector.") # nolint
})
# Test 2: Invalid class type for fields throws error
test_that("invalid class type for fields throws error", {
  expect_error(get_account(list(),
                           encrypted_account_id = "test",
                           fields = 1),
               "Tokens must be a list, encrypted account ID must be a string, and fields must be NULL, a string, or character vector.") # nolint
})
# Test 3: Invalid class type for encrypted_account_id throws error
test_that("invalid class type for encrypted_account_id throws error", {
  expect_error(get_account(list(),
                           encrypted_account_id = "error",
                           fields = 1),
               "Tokens must be a list, encrypted account ID must be a string, and fields must be NULL, a string, or character vector.") # nolint
})
# Test 4: Bad API authentication call throws error
test_that("bad API call throws error", {
  expect_message(get_account(list(),
                             encrypted_account_id = "test",
                             fields = "test"),
               "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application. More specifics on error are below:") # nolint
})
