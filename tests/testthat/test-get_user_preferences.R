# Unit tests for get_user_preferences function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_user_preferences(1), # nolint
               "Tokens parameter must be a list.") # nolint
})
# Test 2: Bad API authentication call throws error
test_that("bad API call throws error", {
  expect_message(get_user_preferences(list()),
               "401 error - authorization token is invalid or there are no accounts allowed to view/use for trading that are registered with the provided third party application. More specifics on error are below:") # nolint
})