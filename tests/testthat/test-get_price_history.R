# Unit tests for get_price_history function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_price_history(1), # nolint
               "Tokens parameter must be a list and symbol must be a string. Also, period type and frequency type must be numeric or NULL, and period and frequency must be numeric or NULL.") # nolint
})
# Test 2: Invalid class type for symbol throws error
test_that("invalid class type for symbol throws error", {
  expect_error(get_price_history(list(),
                                 symbol = TRUE), # nolint
               "Tokens parameter must be a list and symbol must be a string. Also, period type and frequency type must be numeric or NULL, and period and frequency must be numeric or NULL.") # nolint
})
# Test 3: Invalid class type for period type throws error
test_that("invalid class type for period type throws error", {
  expect_error(get_price_history(list(),
                                 symbol = "AAPL",
                                 period_type = 1), # nolint
               "Tokens parameter must be a list and symbol must be a string. Also, period type and frequency type must be numeric or NULL, and period and frequency must be numeric or NULL.") # nolint
})
# Test 4: Invalid class type for period throws error
test_that("invalid class type for period throws error", {
  expect_error(get_price_history(list(),
                                 symbol = "GOOG",
                                 period = "test"), # nolint
               "Tokens parameter must be a list and symbol must be a string. Also, period type and frequency type must be numeric or NULL, and period and frequency must be numeric or NULL.") # nolint
})
# Test 5: Invalid class type for frequency type throws error
test_that("invalid class type for frequency type throws error", {
  expect_error(get_price_history(list(),
                                 symbol = "GOOG",
                                 frequency_type = factor("test")), # nolint
               "Tokens parameter must be a list and symbol must be a string. Also, period type and frequency type must be numeric or NULL, and period and frequency must be numeric or NULL.") # nolint
})
# Test 6: Invalid class type for frequency throws error
test_that("invalid class type for frequency throws error", {
  expect_error(get_price_history(list(),
                                 symbol = "GOOG",
                                 frequency = "hello"), # nolint
               "Tokens parameter must be a list and symbol must be a string. Also, period type and frequency type must be numeric or NULL, and period and frequency must be numeric or NULL.") # nolint
})
# Test 7: Invalid class type for start datetime throws error
test_that("invalid class type for start datetime throws error", {
  expect_error(get_price_history(list(),
                                 symbol = "GOOG",
                                 start_datetime = "hello"), # nolint
               "Start/end dates must be a date and extended hours and previous close must be boolean.") # nolint
})
# Test 8: Invalid class type for end datetime throws error
test_that("invalid class type for end datetime throws error", {
  expect_error(get_price_history(list(),
                                 symbol = "GOOG",
                                 end_datetime = "hello"), # nolint
               "Start/end dates must be a date and extended hours and previous close must be boolean.") # nolint
})
# Test 9: Invalid class type for extended hours throws error
test_that("invalid class type for extended hours throws error", {
  expect_error(get_price_history(list(),
                                 symbol = "GOOG",
                                 need_extended_hours_data = "hello"), # nolint
               "Start/end dates must be a date and extended hours and previous close must be boolean.") # nolint
})
# Test 10: Invalid class type for need previous close throws error
test_that("invalid class type for need previous close throws error", {
  expect_error(get_price_history(list(),
                                 symbol = "GOOG",
                                 need_previous_close = "hello"), # nolint
               "Start/end dates must be a date and extended hours and previous close must be boolean.") # nolint
})
# Test 11: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_price_history(list(),
                                 symbol = "GOOG"), # nolint
               "Error during API call - please check inputs and ensure access token is refreshed.") # nolint
})