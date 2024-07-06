# Unit tests for get_user_preferences function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_user_preferences(1), # nolint
               "Tokens parameter must be a list.") # nolint
})
# Test 2: Bad API throws error
test_that("bad API call throws error", {
  expect_error(get_user_preferences(list()), # nolint
               "Error during call - please check token input object and ensure access token is refreshed.") # nolint
})