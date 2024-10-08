% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/place_order.R
\name{place_order}
\alias{place_order}
\title{Place Order for Specific Account}
\usage{
place_order(tokens, account_number, request_body)
}
\arguments{
\item{tokens}{token object from `get_authentication_tokens` function (list).}

\item{account_number}{encrypted ID of the account (string).}

\item{request_body}{Valid request to API for placing an order (JSON).}
}
\value{
Returns a message informing the user if the order was successfully
        placed/created or if there was an error.
}
\description{
Given the tokens object from the `get_authentication_tokens`
function, the encrypted account ID, and the request body (JSON), place
the specific order. Note that due to the complexity of the orders
that can be created, currently this function allows maximum flexibility
by not cultivating an easier solution to building the request
body and assumes the user passes the appropriate JSON. As
a result, it is strongly encouraged to look at the documentation for how
to build the proper orders for programmatic execution and do robust testing
outside of market hours to ensure that when a live trade comes it
will be just as the user intended. The user of this function assumes
all risk that trades could not be executed exactly as intended as
the API and this package are still under active development.
}
\author{
Nick Bultman, \email{njbultman74@gmail.com}, July 2024
}
\keyword{account}
\keyword{order}
\keyword{place}
