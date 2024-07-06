# Unit tests for get_option_expiration_chain function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_option_expiration_chain(1), # nolint
               "Tokens parameter must be a list and symbol parameter must be a string.") # nolint
})
# Test 2: Invalid class type for symbol throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_option_expiration_chain(list(), 1), # nolint
               "Tokens parameter must be a list and symbol parameter must be a string.") # nolint
})
# Test 3: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_option_expiration_chain(list(), "test"), # nolint
               "Error during call - please check inputs and ensure access token is refreshed.") # nolint
})
