# Unit tests for the get_market_hours_single function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_market_hours_single(1,
                                       market = "test"),
               "Tokens parameter must be a list, market should be a string, and date should be a date.") # nolint
})
# Test 2: Invalid class type for market throws error
test_that("invalid class type for market throws error", {
  expect_error(get_market_hours_single(list(),
                                       market = 1),
               "Tokens parameter must be a list, market should be a string, and date should be a date.") # nolint
})
# Test 3: Invalid value for market throws error
test_that("invalid value for market throws error", {
  expect_error(get_market_hours_single(list(),
                                       market = "error"),
               "Market must be 'equity', 'option', 'bond', 'future', or 'forex'.") # nolint
})
# Test 4: Invalid class type for date throws error
test_that("invalid class type for date throws error", {
  expect_error(get_market_hours_single(list(),
                                       market = "equity",
                                       date = FALSE),
               "Tokens parameter must be a list, market should be a string, and date should be a date.") # nolint
})
# Test 5: Invalid value for date throws error
test_that("invalid value for date throws error", {
  expect_error(get_market_hours_single(list(),
                                       market = "equity",
                                       date = as.Date("2020-01-01")),
               "Date must be today or no greater than one year in the future when not NULL.") # nolint
})
# Test 6: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_market_hours_single(list(),
                                       market = "equity"),
               "Error during API call - please check inputs and ensure access token is refreshed.") # nolint
})