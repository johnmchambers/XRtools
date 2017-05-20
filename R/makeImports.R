#' Write Import Directives for a Package
#'
#' Import directives suitable for all non-local functions called from the specified package will be written
#' to the output (standard output by default), if the function is found in a package currently attached to the
#' session.
#'
#' The function starts with the namespace of the packaage.  It uses `codetools::findGlobals()` to get a list of
#' non-local references.  If `missingOnly` is `TRUE`, any of those found in the parent of the namespace (the
#' imports environment) are omitted.  The chain of parent environments is then searched for corresponding
#' function definitions.  For all found in a package, `importFrom()` directives are constructed.  These
#' are printed to the `output` connection (standard output by default), along with comments reporting
#' those references not found and those found, but not in a package.
#'
#' Two details to note.  The imports environment will contain those objects appearing in imports directives
#' in the current NAMESPACE file. Because the chain of parent environments includes the search path, the
#' relevant packages should be attached in the intended order before calling `makeImports()`.
#'
#' Also, the current implementation does nothing special about calls in the form `package::f()`, so it could
#' give erroneous imports for those functions.  Also, it relies on the `findGlobals()` function, which cannot
#' always find all global references.
#'
#' @param package Either the character string name of the package or an environment.  In the first case, searches
#' will start in the namespace of the environment; in the second they will start in the supplied argument.
#' The character string version is usually what you want.  See Details.
#' @param missingOnly If TRUE, only globals not in the parent environment will be searched for: see the Details
#' for what this means.
#' @param output A connection, standard output by default.  The function doesn't currently handle file names, so
#' you need to open the connection before the call.
#' @return A list of all the global references, split by package name, is returned invisibly.
makeImports <- function(package, missingOnly = TRUE, output = stdout()) {
    pkgOf <- function(what) {
        parEnv <- parent.env(env)
        pkg <- "<NOT FOUND>"
        while(!identical(parEnv, emptyenv())) {
            if(exists(what, envir = parEnv, inherits = FALSE)) {
                obj <- get(what, envir = parEnv)
                objEnv <- if(is.function(obj)) environment(obj) else parEnv
                if(is.null(objEnv)) # a primitive function
                    pkg <- "base"
                else {
                    pkg <- packageName(objEnv)
                    if(is.null(pkg))
                        pkg <- "<NOT PACKAGE>"
                }
                break
            }
            parEnv <- parent.env(parEnv)
        }
        pkg
    }
    if(is(package, "character")) {
        pkgName <- package
        env <- asNamespace(package)
    }
    else { # must be an environment; packageName() checks
        pkgName <- methods::packageName(package)
        if(is.null(pkgName))
            stop('Argument "packge" should be the name or namespace of a package')
        env <- package
    }
    if(isOpen(output))
        con <- output
    else
        con <- NULL
    globals <- character()
    for(what in objects(env)) {
        obj <- get(what, envir = env)
        if(is.function(obj))
            globals <- c(globals, codetools::findGlobals(obj))
    }
    globals <- unique(globals)
    exclude <- unique(c(objects(env, all=TRUE), objects(baseenv(), all = TRUE)))
    if(missingOnly)
        exclude <- unique(c(objects(parent.env(env)), exclude))
    globals <- globals[is.na(match(globals, exclude))]
    if(length(globals)==0)
        return(invisible(list()))
    pkgs <- sapply(globals, pkgOf)
    imports <- split(globals, pkgs)
    pkgs <- names(imports)
    if(isOpen(output))
        for(i in seq_along(imports)) {
            pkg <- pkgs[[i]]
            fromPkg <- imports[[i]]
            needticks <- grep("[^a-zA-Z0-9.]",fromPkg)
            if(length(needticks))
                fromPkg[needticks] <- paste0("`",fromPkg[needticks],"`")
            switch(pkg,
                   "<NOT PACKAGE>" = cat("## Found, but not in package:",
                   paste(fromPkg, collapse = ", "), "\n", file = output),
                   "<NOT FOUND>" = cat("## Not found:",
                   paste(fromPkg, collapse = ", "), "\n", file = output),
                   cat(gettextf("importFrom(%s, %s)", pkg,
                                paste(fromPkg, collapse = ", ")), "\n", file = output))
        }
    invisible(imports)
}
