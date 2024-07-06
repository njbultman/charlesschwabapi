# Unit tests for get_instruments function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_instruments(1,
                               symbol = "test",
                               projection = "test"),
               "Tokens parameter must be a list, symbol should be a string or character vector, and projection should be strings.") # nolint
})
# Test 2: Invalid class type for symbol throws error
test_that("invalid class type for symbol throws error", {
  expect_error(get_instruments(list(),
                               symbol = 1,
                               projection = "test"),
               "Tokens parameter must be a list, symbol should be a string or character vector, and projection should be strings.") # nolint
})
# Test 3: Invalid class type for projection throws error
test_that("invalid class type for projection throws error", {
  expect_error(get_instruments(list(),
                               symbol = "test",
                               projection = 1),
               "Tokens parameter must be a list, symbol should be a string or character vector, and projection should be strings.") #nolint
})
# Test 4: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_instruments(list(),
                               symbol = "test",
                               projection = "symbol-search"),
               "Error during API call - please check token input object and ensure access token is refreshed.") #nolint
})
