# Unit tests for get_accounts function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_accounts(1),
               "Tokens must be a list and fields must be NULL, a string, or character vector.")
})
# Test 2: Invalid class type for fields throws error
test_that("invalid class type for fields throws error", {
  expect_error(get_accounts(list(),
                            fields = 1),
               "Tokens must be a list and fields must be NULL, a string, or character vector.")
})
# Test 3: Bad API authentication call throws error
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_accounts(list(),
                                              fields = "test")),
                regexp = "Unauthorized")
})
