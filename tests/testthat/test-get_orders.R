# Unit tests for the get_orders function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_orders(1),
               "Tokens must be a list, from/to entered times and status must be strings, and max results should be numeric.")
})
# Test 2: Invalid class type for from entered datetime throws error
test_that("invalid class type for from entered datetime throws error", {
  expect_error(get_orders(list(),
                          from_entered_datetime = 1),
               "Tokens must be a list, from/to entered times and status must be strings, and max results should be numeric.")
})
# Test 3: Invalid class type for to entered datetime throws error
test_that("invalid class type for to entered datetime throws error", {
  expect_error(get_orders(list(),
                          to_entered_datetime = 1),
               "Tokens must be a list, from/to entered times and status must be strings, and max results should be numeric.")
})
# Test 4: Invalid class type for max results throws error
test_that("invalid class type for max results throws error", {
  expect_error(get_orders(list(),
                          max_results = "test"),
               "Tokens must be a list, from/to entered times and status must be strings, and max results should be numeric.")
})
# Test 5: Invalid class type for status throws error
test_that("invalid class type for status throws error", {
  expect_error(get_orders(list(),
                          status = FALSE),
               "Tokens must be a list, from/to entered times and status must be strings, and max results should be numeric.")
})
# Test 6: Invalid value for status throws error
test_that("invalid value for status throws error", {
  expect_error(get_orders(list(),
                          status = "test"),
               "Status must be NULL or 'AWAITING_PARENT_ORDER', 'AWAITING_CONDITION', 'AWAITING_STOP_CONDITION', 'AWAITING_MANUAL_REVIEW', 'ACCEPTED', 'AWAITING_UR_OUT', 'PENDING_ACTIVATION', 'QUEUED', 'WORKING', 'REJECTED', 'PENDING_CANCEL', 'CANCELED', 'PENDING_REPLACE', 'REPLACED', 'FILLED', 'EXPIRED', 'NEW', 'AWAITING_RELEASE_TIME', 'PENDING_ACKNOWLEDGEMENT', 'PENDING_RECALL', or 'UNKNOWN'.")
})
# Test 7: Bad API authentication call throws error
test_that("bad API call returns error", {
  expect_output(suppressMessages(get_orders(list())),
                regexp = "Unauthorized")
})
