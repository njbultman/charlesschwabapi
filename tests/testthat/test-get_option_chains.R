# Unit tests for get_option_chains function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_option_chains(1, symbol = "test"),
               "Tokens must be a list and strike count, interval, and strike must be NULL or numeric, and range & option type must be NULL or character.")
})
# Test 2: Invalid class type for interval throws error
test_that("invalid class type for strike_count throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 strike_count = FALSE),
               "Tokens must be a list and strike count, interval, and strike must be NULL or numeric, and range & option type must be NULL or character.")
})
# Test 3: Invalid class type for interval throws error
test_that("invalid class type for interval throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 interval = FALSE),
               "Tokens must be a list and strike count, interval, and strike must be NULL or numeric, and range & option type must be NULL or character.")
})
# Test 4: Invalid class type for strike throws error
test_that("invalid class type for strike throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 strike = as.factor(3)),
               "Tokens must be a list and strike count, interval, and strike must be NULL or numeric, and range & option type must be NULL or character.")
})
# Test 5: Invalid class type for range throws error
test_that("invalid class type for range throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 range = FALSE),
               "Tokens must be a list and strike count, interval, and strike must be NULL or numeric, and range & option type must be NULL or character.")
})
# Test 6: Invalid class type for option type throws error
test_that("invalid class type for option type throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 option_type = FALSE),
               "Tokens must be a list and strike count, interval, and strike must be NULL or numeric, and range & option type must be NULL or character.")
})
# Test 7: Invalid class type for from date throws error
test_that("invalid class type for from date throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 from_date = FALSE),
               "To/from dates must be dates or NULL.")
})
# Test 8: Invalid class type for to date throws error
test_that("invalid class type for to date throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 to_date = FALSE),
               "To/from dates must be dates or NULL.")
})
# Test 9: Invalid value for strategy throws error
test_that("invalid value for strategy throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 strategy = "error"),
               "Strategy must be NULL or 'SINGLE', 'ANALYTICAL', 'COVERED', 'VERTICAL', 'CALENDAR', 'STRANGLE', 'STRADDLE', 'BUTTERFLY', 'CONDOR', 'DIAGONAL', 'COLLAR', or 'ROLL'.")
})
# Test 10: Invalid value for expiration month throws error
test_that("invalid value for expiration month throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 exp_month = "error"),
               "Expiration month must be NULL or 'ALL', 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', or 'DEC'.")
})
# Test 11: Invalid value for contract type throws error
test_that("invalid value for contract type throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 contract_type = "error"),
               "Contract type must be NULL or 'ALL', 'CALL', or 'PUT'.")
})
# Test 12: Invalid value for entitlement throws error
test_that("invalid value for entitlement throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 entitlement = "error"),
               "Entitlement must be NULL or 'PP', 'NP', or 'PN'.")
})
# Test 13: Invalid value for volatility when strategy is ANALYTICAL throws error
test_that("invalid value for volatility when strategy is ANALYTICAL throws error", {
  expect_error(get_option_chains(list(),
                                 symbol = "test",
                                 strategy = "SINGLE",
                                 volatility = "test"),
               "Volatility, underlying price, interest rate, and days to expiration should only be non-NULL when strategy is ANALYTICAL.")
})
# Test 14: Bad API authentication call throws error
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_option_chains(list(),
                                                   symbol = "AAPL")),
                regexp = "InvalidAccessToken")
})
