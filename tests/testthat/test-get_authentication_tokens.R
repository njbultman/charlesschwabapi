# Unit tests for get_authentication function

# Test 1: Invalid class type for app_key throws error
test_that("invalid class type for app_key throws error", {
  expect_error(get_authentication_tokens(app_key = 1, redirect_uri = "test", app_secret = "test"), # nolint
               "app_key, redirect_uri, app_secret, and token_save_path must all be strings.") # nolint
})
# Test 2: Invalid class type for redirect_uri throws error
test_that("invalid class type for redirect_uri throws error", {
  expect_error(get_authentication_tokens(app_key = "test", redirect_uri = 1, app_secret = "test"), # nolint
               "app_key, redirect_uri, app_secret, and token_save_path must all be strings.") # nolint
})
# Test 3: Invalid class type for app_secret throws error
test_that("invalid class type for app_secret throws error", {
  expect_error(get_authentication_tokens(app_key = "test", redirect_uri = "test", app_secret = TRUE), # nolint
               "app_key, redirect_uri, app_secret, and token_save_path must all be strings.") # nolint
})
# Test 4: Invalid class type for token_save_path throws error
test_that("invalid class type for app_secret throws error", {
  expect_error(get_authentication_tokens(app_key = "test", redirect_uri = "test", app_secret = "test", token_save_path = TRUE), # nolint
               "app_key, redirect_uri, app_secret, and token_save_path must all be strings.") # nolint
})
# Test 5: If Renv has RUN_SCHWAB_LIVE=TRUE
test_that("check for a login", { 
  skip_if_not(Sys.getenv("RUN_SCHWAB_LIVE", "FALSE") == "TRUE", "Do not attempt the actual login.")  
  tokens <- get_authentication_tokens(
      Sys.getenv("SCHWAB_APP_KEY"),
      Sys.getenv("SCHWAB_REDIRECT_URI"),
      Sys.getenv("SCHWAB_SECRET"),
      getwd() )
  expect_equal(tokens$token_type, "Bearer")
})
