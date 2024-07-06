# Unit tests for the get_market_hours function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_market_hours(1,
                                markets = "test"),
               "Tokens parameter must be a list, markets should be a character vector or string, and date should be a date.") # nolint
})
# Test 2: Invalid class type for market throws error
test_that("invalid class type for market throws error", {
  expect_error(get_market_hours(list(),
                                markets = 1),
               "Tokens parameter must be a list, markets should be a character vector or string, and date should be a date.") # nolint
})
# Test 3: Invalid value for markets throws error
test_that("invalid value for market throws error", {
  expect_error(get_market_hours(list(),
                                markets = "error"),
               "Markets must be 'equity', 'option', 'bond', 'future', and/or 'forex'.") # nolint
})
# Test 4: Invalid class type for date throws error
test_that("invalid class type for date throws error", {
  expect_error(get_market_hours(list(),
                                markets = "equity",
                                date = FALSE),
               "Tokens parameter must be a list, markets should be a character vector or string, and date should be a date.") # nolint
})
# Test 5: Invalid value for date throws error
test_that("invalid value for date throws error", {
  expect_error(get_market_hours(list(),
                                markets = "equity",
                                date = as.Date("2020-01-01")),
               "Date must be today or no greater than one year in the future when not NULL.") # nolint
})
# Test 6: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_market_hours(list(),
                                markets = "equity"),
               "Error during API call - please check inputs and ensure access token is refreshed.") # nolint
})
