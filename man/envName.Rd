\name{envName}
\alias{envName}
\alias{envNames}
\title{Informative Names for Environments}
\usage{
envName(envir)

envNames(envir)
}
\arguments{
\item{envir}{An environment object.}
}
\value{
A character vector, with the single name of the environment or that followed in
order by its successive parents; i.e., \code{parent.env(envir)}, \code{parent.env(parent.env(envir))}, etc.
}
\description{
These functions return the names for an environment (\code{envName()}) or
for an environment and all its parents (\code{envNames()}), with informative
labels in the style of the default print method, but extended to describe
evaluation frames as well as namespaces and package environments.
}
\author{
John Chambers
}
\examples{
## the namespace for this package, and its parents
envNames(asNamespace("XR"))
# a function that returns its frame and parents
showMyEnv <- function() envNames(sys.frame(sys.nframe()))
showMyEnv()
}

\keyword{programming}
