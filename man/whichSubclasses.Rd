\name{whichSubclasses}
\alias{whichSubclasses}
\alias{showVectorTypes}
\title{
Find Selected Subclasses of a Class
}
\description{
Returns the names of subclasses of a given class, restricted
optionally to those of a particular distance (especially \code{1} for
direct subclasses) and to those that are or are not simple extensions.
\code{showVectorTypes} is just an example that prints all the types of
all the basic
vector classes.
}
\usage{
whichSubclasses(Class, distance = NA, simple = NA)
showVectorTypes(withClasses)

}
\arguments{
  \item{Class}{
The name of the class or its formal definition, as returned by \code{\link{getClass}()}.
}
  \item{distance}{
The range of distance (\code{1} == direct subclasses, \code{2} ==
their direct subclasses, etc.).  By default, no restriction.
}
  \item{simple}{
Should these be simple subclasses, that is, those created by the
\code{contains=} argument, without any explicit function to extract or
replace the superclass?  Legal values: \code{NA} (no restriction);
\code{TRUE} or \code{FALSE}.}
\item{withClasses}{In \code{showVectorTypes()}, should the
    corresponding clases be printed?}
}
\value{
The character vector of names of the selected subclasses.
}
\author{
John Chambers
}
\examples{
## the essential implementation of showVectorTypes()
classes <- whichSubclasses("vector", distance = 1, simple = TRUE)
sapply(classes, function(x) typeof(new(x)))
}

\keyword{ classes }
