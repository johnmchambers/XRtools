\name{descriptionHints}
\alias{descriptionHints}
\title{
Check NAMESPACE and DESCRIPTION files for requirements
}
\description{
The function compares the imports and dynamic library requirements
from the package's NAMESPACE file with the directives in the
DESCRIPTION file, and prints a suggestion if some requirements seem to
be missing.
}
\usage{
descriptionHints(package)
}
\arguments{
  \item{package}{
The quoted file path to the source package's directory.  Files
\code{"DESCRIPTION"} and \code{"NAMESPACE"} should exist in this
directory.
}
}
\details{
The \code{"NAMESPACE"} file is parsed and the various import and
dynamic library requirements are processed to give a list of the
apparently needed packages.  This list is compared to the
\code{"Depends"} and \code{"Imports"} directives
in the \code{"DESCRIPTION"} file.  If some dependencies are missing, the function prints a
message with a suggested line for the \code{"DESCRIPTION"} file.
}
\value{
Returns invisibly the suggested dependencies, if these were not provided.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
John M. Chambers
}
\note{
In addition to the dependencies noted here, the current version of R
(2.14.0) requires \code{"Depends: methods"} for the \code{check}
utility if the package uses formal methods and classes.
}
\keyword{ programming }
\keyword{ packages }
