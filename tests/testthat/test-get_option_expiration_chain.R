# Unit tests for get_option_expiration_chain function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_option_expiration_chain(1),
               "Tokens parameter must be a list and symbol parameter must be a string.")
})
# Test 2: Invalid class type for symbol throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_option_expiration_chain(list(), 1),
               "Tokens parameter must be a list and symbol parameter must be a string.")
})
# Test 3: Bad API authentication call throws error
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_option_expiration_chain(list(),
                                                             symbol = "AAPL")),
                regexp = "InvalidAccessToken")
})
