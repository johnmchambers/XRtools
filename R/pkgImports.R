pkgImports <- function(pkg, destdir = ".") {
    got <- download.packages(pkg, destdir, type = "source")
    if(nrow(got) < length(pkg))
        return(character())
    file <- got[1,2]
    untar(file, exdir = destdir)
    dfile <- file.path(destdir, pkg, "DESCRIPTION")
    info <- read.dcf(dfile)
    if("Imports" %in% colnames(info)) {
        value <- strsplit(info[,"Imports"], ",[ \n]*")[[1]]
        gsub(" .*$", "", value) # remove version qualifiers
    }
    else
        character()
}

#' Download Source for Packages Importing othe Specified Packages
#'
#' Given the names of candidate packages in \code{pkgs}, this function
#' downloads the source from the default repository.  Chosen packages
#' are expanded into directories under \code{destdir}, by default
#' the current working directory.  If \code{mustuse} is supplied, only
#' packages specifying one of these in their \code{"Imports"} directive
#' will be retained.
#'
#' @param pkgs The names of the candidate packages.
#' @param mustUse The names of the required packages; only those candidates
#' having one or more of these in the \code{"Imports"} directive are retained.
#' Note that the packages are not installed; only presence in the directive is tested.
#' @param destdir The directory under which subdirectories will be created for the
#' retained packages, containing the package source.  These are created by expanding
#' the \dQuote{"tar.gz"} file.
#' @return The subset of \code{pkgs} retained.
pkgsUsing <- function(pkgs, mustUse = character(), destdir = ".") {
    value <- character()
    for(pkg in pkgs) {
        imports <- pkgImports(pkg, destdir)
        what <- file.path(destdir, pkg)
        system(gettextf("rm %s*.gz", what))
        if(length(mustUse) && !all(mustUse %in% imports))
            system(gettextf("rm -rf %s", what))
        else
            value <- c(value, pkg)
    }
    value
}
