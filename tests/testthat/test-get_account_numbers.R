# Unit tests for get_account_numbers function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_account_numbers(1),
               "Tokens parameter must be a list.")
})
# Test 2: Bad API authentication call returns output
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_account_numbers(list())),
                regexp = "Unauthorized")
})
