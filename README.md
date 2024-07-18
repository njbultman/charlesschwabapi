# charlesschwabapi

## R Package Wrapper Around 'Charles Schwab Individual Trader API'

### Installation
Install release version from CRAN:

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/charlesschwabapi)](https://cran.r-project.org/package=sleeperapi)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/charlesschwabapi)](https://www.r-pkg.org/services)
[![R-CMD-check](https://github.com/njbultman/charlesschwabapi/workflows/R-CMD-check/badge.svg)](https://github.com/njbultman/charlesschwabapi/actions) 
[![Codecov test coverage](https://codecov.io/gh/njbultman/charlesschwabapi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/njbultman/charlesschwabapi?branch=main)

```R
install.packages("charlesschwabapi")
library(charlesschwabapi)
```

Install latest development version through GitHub via `devtools` package:
```R
install.packages("devtools") # If have not installed package already
library(devtools)
devtools::install_github("njbultman/charlesschwabapi")
```

### Introduction
For those wishing to interact with the [Charles Schwab Individual Trader API](https://developer.schwab.com/products/trader-api--individual) with R in a simplified manner, this package offers wrapper functions around authentication and the available API calls to streamline the process.

The `Authentication` section below will walk through the first steps on initially authenticating and then reauthenticating as time passes. This package has been tested when using the localhost callback URL as that is what many users of the Individual Trader API will use. After authenticating, one can use the `tokens` RDS object returned to pass into other functions in this package.

### Disclosure
This software is in no way affiliated, endorsed, or approved by Charles Schwab or any of its affiliates. It comes with absolutely no warranty and should not be used in or for actual trading unless the user can read and understand the source code. The functions within this package have been tested under basic scenarios. There may be bugs or issues that could prevent a user from executing trades, replacing trades, or canceling trades. It is also possible trades could be submitted in error. The user will use this package at their own risk.

Please note the following warning related to the `place_order` and `replace_order` functions: THE `place_order` AND `replace_order` FUNCTIONS HAVE BEEN TESTED ONLY UNDER VERY LIMITED SCENARIOS WHEN THERE ARE NUMEROUS POTENTIAL COMBINTATIONS. CHARLES SCHWAB HAS THEIR OWN ERROR HANDLING BEHIND THE SCENES BUT A SUCCESSFUL COMBINATION INPUTTED INTO THESE FUNCTIONS COULD BE EXECUTED IMMEDIATELY AND HAVE FINANCIAL CONSEQUENCES. VERIFY ALL ENTRIES BEFORE SUBMITTING AND IT IS STRONGLY ADVISED TO TEST THE DESIRED ORDER(S) FIRST. THREE WAYS THIS COULD BE ACHIEVED ARE AS FOLLOWS:

1. Use these functions outside of normal trading hours and ensure on the Charles Schwab applications that they are as intended.
2. Use limit orders with limit prices far outside the current bid/ask.
3. Enter small quantities to ensure not much capital is at risk.

FOR OPTIONS ONE AND TWO, PLEASE BE SURE TO CANCEL THEM USING THE `cancel_order` FUNCTION OR BY OTHER MEANS THROUGH THE CHARLES SCHWAB APPLICATIONS.

### Authentication
Before doing anything related to this package, the user must first create a profile and application with Charles Schwab's Individual Trader API at their developer [website](https://developer.schwab.com/). While all the specifics will not be gone through here, there are many excellent tutorials to walk one through this setup process. For example `quantRoom` has an excellent YouTube video [here](https://www.youtube.com/watch?v=AOiFYj5iM5U&t=2s) showing how to create an application with the Charles Schwab Individual Trader API using the localhost callback URL and ensuring both the accounts/trading and market data functionality are available. The first minute contains the relevant information. Once this is complete and the application is approved, one will be provided with the application key, application secret, and the redirect URI (callback URL) needed to start using the package.

After the application is created, one can initialize an object (for example `tokens`) and call the `get_authentication_tokens` function and passing in the application key, redirect URI (callback URL), application secret, and an optional save path for the tokens (it will default to saving in the current working directory). It is recommended from a security perspective to place the application key and application secret in environment variables and then put those in the script rather than putting them in directly, but both methods will suffice with authentication.
```R
# Define requirements for authentication
app_key <- "YOUR APPLICATION KEY"
redirect_uri <- "YOUR REDIRECT URI (CALLBACK URL)"
app_secret <- "YOUR APPLICATION SECRET"
token_save_path <- "/savePath/to/tokenfile" # Optional but recommended to place somewhere intentionally

tokens <- charlesschwabapi::get_authentication_tokens(app_key, redirect_uri, app_secret, token_save_path)
```
If this is the first time authenticating, the function will attempt to open a site on the browser of choice to enter login credentials with Charles Schwab. Simply enter the login credentials and follow the other steps to accept the terms and indicate which account(s) to be granted access for the API. After everything is done, one will be directed to a blank page. Before exiting that page, copy the URL at the top of the browser window and paste it into the R console and press enter. After that is complete, a message will inform the user that authentication was successful and the tokens have been saved at the specified path (or the working directory by default) and in the object assigned to the function call. For those a bit confused/curious by the browser section and needing to enter credentials and follow other steps before copying/pasting the URL into the console, there are tutorial videos available that walk through this (albeit at a more granular level since the function takes care of the specifics). Again, `quantRoom` has an excellent video showing this [here](https://www.youtube.com/watch?v=P2aYY9CiRLM&t=19s). The first 2 minutes will be the most relevant.

At this point, authentication is complete, and the object defined through the `get_authentication_tokens` function will have everything needed to call other functions properly and reauthenticate. Additionally, the RDS object saved can brought back in with a new console session via `readRDS` or `get_authentication_tokens` (the latter being recommended as explained shortly). It is important that when reauthenticating, be sure to use the same file path that the initial tokens were saved to. Failure to do so will mean that manual authentication (meaning repeating the same steps above) will be required every time a new console session is started when this is not necessary. Currently the refresh token expires after 30 minutes and the access token expires after 7 days. Put differently, after initial authentication, one will only need to go through the manual authentication method after 7 days and a more automated refresh can be used after 30 minutes by simply calling the `get_authentication_tokens` function again with the same parameters and setting it to the same object initially defined. The function will automatically reauthenticate and one will be good for another 30 minutes before needing to reauthenticate again.
```R
# To reauthenticate after 30 minutes but before 7 days have passed since initial authentication
# Be sure token_save_path is same that was used for initial authentication
# If access token still valid and only refresh token expired, will automatically reauthenticate
# After 7 days, manual authentication will need to take place again

tokens <- charlesschwabapi::get_authentication_tokens(app_key, redirect_uri, app_secret, token_save_path)
```

### Basic Usage
Now that authentication is situated, the object with token information (`tokens` above) can be used for other functions to gather data. For example, to get account information, one can call the `get_account_numbers` function and pass in the token information object (`tokens`) to return a data frame containing information related to the authenticated user's accounts.
```R
# Get account number information
account_numbers <- charlesschwabapi::get_account_numbers(tokens)
```
While account number information is important to know, it is particularly useful when one wants to obtain the encrypted ID of the account to pass to other functions requiring it. For example, the `get_orders_account` function requires the encrypted account, so the information obtained from `get_account_numbers` can be used for `get_orders_account`.
```R
# Obtain encrypted account ID for first account in account_numbers data frame
account_number <- account_numbers$hashValue[1]

# Then, this value can be passed into get_orders_account
account_orders <- charlesschwabapi::get_orders_account(tokens, account_number)
```
There are many additional functions available to gather not only information about the user's accounts but also general market information (quotes, option chains, price history, and more). It is encouraged to look through all the documentation pertaining to each function and the documentation related to the Individual Trader API on the Charles Schwab developer site to fully explore and understand the capabilities to make the most of this package.
