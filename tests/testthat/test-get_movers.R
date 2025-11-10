# Unit tests for get_movers function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_movers(1, symbol_id = "test"),
               "Tokens parameter must be a list, symbol and sort must be a strings, and frequency must be numeric.")
})
# Test 2: Invalid class type for symbol ID throws error
test_that("invalid class type for symbol ID throws error", {
  expect_error(get_movers(list(), symbol_id = 1),
               "Tokens parameter must be a list, symbol and sort must be a strings, and frequency must be numeric.")
})
# Test 3: Invalid class type for sort throws error
test_that("invalid class type for sort throws error", {
  expect_error(get_movers(list(),
                          symbol_id = "test",
                          sort = 1),
               "Tokens parameter must be a list, symbol and sort must be a strings, and frequency must be numeric.")
})
# Test 4: Invalid class type for frequency throws error
test_that("invalid class type for frequency throws error", {
  expect_error(get_movers(list(),
                          symbol_id = "test",
                          frequency = "error"),
               "Tokens parameter must be a list, symbol and sort must be a strings, and frequency must be numeric.")
})
# Test 4: Invalid value for frequency throws error
test_that("invalid value for frequency throws error", {
  expect_error(get_movers(list(),
                          symbol_id = "$DJI",
                          frequency = 1000),
               "Frequency must be 0, 1, 5, 10, 30, or 60.")
})
# Test 5: Invalid value for sort throws error
test_that("invalid value for sort throws error", {
  expect_error(get_movers(list(),
                          symbol_id = "$DJI",
                          sort = "test"),
               "Sort must be 'VOLUME', 'TRADES', 'PERCENT_CHANGE_UP', or 'PERCENT_CHANGE_DOWN'.")
})
# Test 6: Bad API authentication call throws error
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_movers(list(),
                                            symbol_id = "$DJI")),
                regexp = "InvalidAccessToken")
})