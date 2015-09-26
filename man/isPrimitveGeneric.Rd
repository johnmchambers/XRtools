\name{isPrimitiveGeneric}
\alias{isPrimitiveGeneric}
\title{
Test whether a Primitive Function is an S3 Generic
}
\description{
Given a primitive function from the base package, or its name, this
function carries out an empirical test of whether S3 methods can be
written for the primitive and returns \code{TRUE} or \code{FALSE}
accordingly.
}
\usage{
isPrimitiveGeneric(what)
}
\arguments{
  \item{what}{
The primitive function or its character string name.  Any other object
as argument just returns \code{FALSE}.
}
}
\details{
A tentative method for the function is constructed in the global
environment, a call is generated with an object having the test class
as its class attribute, and the result is compared to the value
returned by the method.
The method is removed on exit.
Since the test is done using \code{\link{tryCatch}()}, errors just
cause \code{FALSE} to be returned.

Not all primitives can be tested with 1 or 2 arguments, and some have
extra requirements before dispatching.  The implementation of this
function has a list of special cases.  Any updates to that list would
be appreciated. 
}
\value{
 \code{TRUE} or \code{FALSE} as described.
}
\author{
John M. Chambers
}
\examples{
isPrimitiveGeneric("[") # TRUE
isPrimitiveGeneric(sin) # TRUE
isPrimitiveGeneric(c) # TRUE
isPrimitiveGeneric(split) # FALSE, not a primitive
isPrimitiveGeneric(is.null) # FALSE, methods not recognized
isPrimitiveGeneric(is.numeric) # TRUE
}
\keyword{ programming }
\keyword{ methods }
