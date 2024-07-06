# Unit tests for get_quotes function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_quotes(tokens = 0,
                          symbols = "AAPL"),
               "Tokens parameter must be a list, symbols and fields must be a string or character vector, and indicative must be a string.") # nolint
})
# Test 2: Invalid class type for symbols throws error
test_that("invalid class type for symbols throws error", {
  expect_error(get_quotes(tokens = list(),
                          symbols = 0),
               "Tokens parameter must be a list, symbols and fields must be a string or character vector, and indicative must be a string.") # nolint
})
# Test 3: Invalid class type for fields throws error
test_that("invalid class type for fields throws error", {
  expect_error(get_quotes(tokens = list(),
                          symbols = "AAPL",
                          fields = 0),
               "Tokens parameter must be a list, symbols and fields must be a string or character vector, and indicative must be a string.") # nolint
})
# Test 4: Invalid class type for indicative throws error
test_that("invalid class type for indicative throws error", {
  expect_error(get_quotes(tokens = list(),
                          symbols = "AAPL",
                          indicative = 0),
               "Tokens parameter must be a list, symbols and fields must be a string or character vector, and indicative must be a string.") # nolint
})
# Test 5: Fields vector containing "all" and others throws error
test_that("fields vector containing 'all' and others throws error", {
  expect_error(get_quotes(tokens = list(),
                          symbols = "AAPL",
                          fields = c("all", "quote")),
               "Fields parameter must be NULL, 'all' or combination of these: 'quote', 'fundamental', 'extended', 'reference', or 'regular'.") # nolint
})
# Test 6: Fields vector containing unnaceptable values throws error
test_that("fields vector containing unnaceptable values throws error", {
  expect_error(get_quotes(tokens = list(),
                          symbols = "AAPL",
                          fields = c("quote", "test")),
               "Fields parameter must be NULL, 'all' or combination of these: 'quote', 'fundamental', 'extended', 'reference', or 'regular'.") # nolint
})
# Test 7: Fields vector containing unnaceptable values throws error
test_that("fields vector containing unnaceptable values throws error", {
  expect_error(get_quotes(tokens = list(),
                          symbols = "AAPL",
                          fields = "test"),
               "Fields parameter must be NULL, 'all' or combination of these: 'quote', 'fundamental', 'extended', 'reference', or 'regular'.") # nolint
})
# Test 8: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_quotes(tokens = list(),
                          symbols = "test"),
               "Error during call - please check inputs and ensure access token is refreshed.") # nolint
})
