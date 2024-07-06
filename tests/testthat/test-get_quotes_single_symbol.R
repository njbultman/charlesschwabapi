# Unit tests for get_quotes_single_symbol_single_symbol function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_quotes_single_symbol(tokens = 0,
                                        symbol_id = "AAPL"),
               "Tokens parameter must be a list while symbol_id must be a string. Fields must be NULL, a string, or a character vector.") # nolint
})
# Test 2: Invalid class type for symbol_id throws error
test_that("invalid class type for symbol_id throws error", {
  expect_error(get_quotes_single_symbol(tokens = list(),
                                        symbol_id = 0),
               "Tokens parameter must be a list while symbol_id must be a string. Fields must be NULL, a string, or a character vector.") # nolint
})
# Test 3: Invalid class type for fields throws error
test_that("invalid class type for fields throws error", {
  expect_error(get_quotes_single_symbol(tokens = list(),
                                        symbol_id = "AAPL",
                                        fields = 0),
               "Tokens parameter must be a list while symbol_id must be a string. Fields must be NULL, a string, or a character vector.") # nolint
})
# Test 4: Fields vector containing "all" and others throws error
test_that("fields vector containing 'all' and others throws error", {
  expect_error(get_quotes_single_symbol(tokens = list(),
                                        symbol_id = "AAPL",
                                        fields = c("all", "quote")),
               "Fields parameter must be NULL, 'all' or combination of these: 'quote', 'fundamental', 'extended', 'reference', or 'regular'.") # nolint
})
# Test 5: Fields vector containing unnaceptable values throws error
test_that("fields vector containing unnaceptable values throws error", {
  expect_error(get_quotes_single_symbol(tokens = list(),
                                        symbol_id = "AAPL",
                                        fields = c("quote", "test")),
               "Fields parameter must be NULL, 'all' or combination of these: 'quote', 'fundamental', 'extended', 'reference', or 'regular'.") # nolint
})
# Test 6: Fields vector containing unnaceptable values throws error
test_that("fields vector containing unnaceptable values throws error", {
  expect_error(get_quotes_single_symbol(tokens = list(),
                                        symbol_id = "AAPL",
                                        fields = "test"),
               "Fields parameter must be NULL, 'all' or combination of these: 'quote', 'fundamental', 'extended', 'reference', or 'regular'.") # nolint
})
# Test 7: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_quotes_single_symbol(tokens = list(),
                                        symbol_id = "test"),
               "Error during call - please check inputs and ensure access token is refreshed.") # nolint
})
