# Unit tests for get_instruments_cusip function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_instruments_cusip(1,
                                     cusip_id = "test"),
               "Tokens parameter must be a list and cusip must be a string.")
})
# Test 2: Invalid class type for cusip throws error
test_that("invalid class type for cusip throws error", {
  expect_error(get_instruments_cusip(list(),
                                     cusip_id = 1),
               "Tokens parameter must be a list and cusip must be a string.")
})
# Test 3: Bad API authentication call throws error
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_instruments_cusip(list(),
                                                       cusip_id = "test")),
                regexp = "InvalidAccessToken")
})
