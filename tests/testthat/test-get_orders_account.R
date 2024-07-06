# Unit tests for the get_orders_account function

# Test 1: Invalid class type for tokens throws error
test_that("invalid class type for tokens throws error", {
  expect_error(get_orders_account(1,
                                  account_number = "test"), # nolint
               "Tokens must be a list, account number, from/to entered times, and status must be strings, and max results should be numeric.") # nolint
})
# Test 2: Invalid class type for from entered datetime throws error
test_that("invalid class type for from entered datetime throws error", {
  expect_error(get_orders_account(list(),
                                  account_number = "test",
                                  from_entered_datetime = 1), # nolint
               "Tokens must be a list, account number, from/to entered times, and status must be strings, and max results should be numeric.") # nolint
})
# Test 3: Invalid class type for to entered datetime throws error
test_that("invalid class type for to entered datetime throws error", {
  expect_error(get_orders_account(list(),
                                  account_number = "test",
                                  to_entered_datetime = 1), # nolint
               "Tokens must be a list, account number, from/to entered times, and status must be strings, and max results should be numeric.") # nolint
})
# Test 4: Invalid class type for max results throws error
test_that("invalid class type for max results throws error", {
  expect_error(get_orders_account(list(),
                                  account_number = "test",
                                  max_results = "test"), # nolint
               "Tokens must be a list, account number, from/to entered times, and status must be strings, and max results should be numeric.") # nolint
})
# Test 5: Invalid class type for status throws error
test_that("invalid class type for status throws error", {
  expect_error(get_orders_account(list(),
                                  account_number = "test",
                                  status = FALSE), # nolint
               "Tokens must be a list, account number, from/to entered times, and status must be strings, and max results should be numeric.") # nolint
})
# Test 6: Invalid value for status throws error
test_that("invalid value for status throws error", {
  expect_error(get_orders_account(list(),
                                  account_number = "test",
                                  status = "test"), # nolint
               "Status must be NULL or 'AWAITING_PARENT_ORDER', 'AWAITING_CONDITION', 'AWAITING_STOP_CONDITION', 'AWAITING_MANUAL_REVIEW', 'ACCEPTED', 'AWAITING_UR_OUT', 'PENDING_ACTIVATION', 'QUEUED', 'WORKING', 'REJECTED', 'PENDING_CANCEL', 'CANCELED', 'PENDING_REPLACE', 'REPLACED', 'FILLED', 'EXPIRED', 'NEW', 'AWAITING_RELEASE_TIME', 'PENDING_ACKNOWLEDGEMENT', 'PENDING_RECALL', or 'UNKNOWN'.") # nolint
})
# Test 7: Bad API call throws error
test_that("bad API call throws error", {
  expect_error(get_orders_account(list(),
                                  account_number = "test",
                                  status = "CANCELED"), # nolint
               "Error during API call - please check inputs and ensure access token is refreshed.") # nolint
})
