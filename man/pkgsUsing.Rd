% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/pkgImports.R
\name{pkgsUsing}
\alias{pkgsUsing}
\title{Download Source for Packages Importing othe Specified Packages}
\usage{
pkgsUsing(pkgs, mustUse = character(), destdir = ".")
}
\arguments{
\item{pkgs}{The names of the candidate packages.}

\item{mustUse}{The names of the required packages; only those candidates
having one or more of these in the \code{"Imports"} directive are retained.
Note that the packages are not installed; only presence in the directive is tested.}

\item{destdir}{The directory under which subdirectories will be created for the
retained packages, containing the package source.  These are created by expanding
the \dQuote{"tar.gz"} file.}
}
\value{
The subset of \code{pkgs} retained.
}
\description{
Given the names of candidate packages in \code{pkgs}, this function
downloads the source from the default repository.  Chosen packages
are expanded into directories under \code{destdir}, by default
the current working directory.  If \code{mustuse} is supplied, only
packages specifying one of these in their \code{"Imports"} directive
will be retained.
}

