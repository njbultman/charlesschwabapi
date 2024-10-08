% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_order.R
\name{replace_order}
\alias{replace_order}
\title{Replace Order for Specific Account}
\usage{
replace_order(tokens, account_number, order_id, request_body)
}
\arguments{
\item{tokens}{token object from `get_authentication_tokens` function (list).}

\item{account_number}{encrypted ID of the account (string).}

\item{order_id}{order ID to be replaced (numeric).}

\item{request_body}{Valid request to API for replacing an order (JSON).}
}
\value{
Returns a message informing the user if the order was successfully
        replaced/created or if there was an error.
}
\description{
Given the tokens object from the `get_authentication_tokens`
function, the encrypted account ID, the order ID and the
request body, replace the specific order. Due to the complexity of the orders
that can be created/replaced, currently this function allows
maximum flexibility by not cultivating an easier solution
to building the request body and assumes the user passes
the appropriate JSON. Much like the `place_order` function,
it is strongly encouraged to look at the documentation (in this package and
on the Charles Schwab developer site) for how to build proper orders before
attempting to replace any. The user of this function assumes
all risk that trades could not be replaced (and then executed)
exactly as intended as the API and this package are still under
active development.
}
\author{
Nick Bultman, \email{njbultman74@gmail.com}, July 2024
}
\keyword{account}
\keyword{order}
\keyword{replace}
