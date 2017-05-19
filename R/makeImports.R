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
