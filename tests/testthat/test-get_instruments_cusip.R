# Unit tests for get_instruments_cusip function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_instruments_cusip(1,
                                     cusip_id = "test"),
               "Tokens parameter must be a list and cusip must be a string.") # nolint
})
# Test 2: Invalid class type for cusip throws error
test_that("invalid class type for cusip throws error", {
  expect_error(get_instruments_cusip(list(),
                                     cusip_id = 1),
               "Tokens parameter must be a list and cusip must be a string.") # nolint
})
# Test 3: Bad API authentication call throws error
test_that("bad API authentication call throws error", {
  expect_message(get_instruments_cusip(list(),
                                       cusip_id = "test"),
               "401 error - authorization token is invalid. More specifics on error are below:") # nolint
})
