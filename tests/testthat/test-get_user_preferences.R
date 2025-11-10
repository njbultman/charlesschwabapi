# Unit tests for get_user_preferences function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_user_preferences(1),
               "Tokens parameter must be a list.")
})
# Test 2: Bad API authentication call throws error
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_user_preferences(list())),
                regexp = "Unauthorized")
})