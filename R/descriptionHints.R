descriptionHints <- function(package) {

    importPkgs <- function(info) {
        if(length(info))
            sapply(info, function(x) if(is.character(x)) x else x[[1]])
        else
            character()
    }

    dependsPkgs <- function(info) {
        if(length(info))
            sapply(info, function(x) x$name)
        else
            character()
    }

    pkgdir <- dirname(package)
    ## parse the namespace to get imports, dynlibs
    nn <- parseNamespaceFile(basename(package), pkgdir, FALSE)
    dd <- tools:::.read_description(file.path(package, "DESCRIPTION"))
    descr <- tools:::.split_description(dd)
    dynlibs <- nn$dynlibs
    if(length(dynlibs))
        dynlibs <- dynlibs[is.na(match(dynlibs, dd[["Package"]]))]
    needPkgs <- unique(c(importPkgs(nn$imports), importPkgs(nn$importClasses),
                         dynlibs))
    dpds <- dependsPkgs(descr$Depends)
    imps <- dependsPkgs(descr$Imports)
    dpdsOrImps <- unique(c(dpds, imps))
    if(any(is.na(match(needPkgs, dpdsOrImps)))) {
        pkgs <- unique(c(dpdsOrImps, needPkgs))
        impLine <- paste( pkgs, collapse = ",", sep = "")
        message(gettextf("Some dependencies were not found in the DESCRIPTION file.
To satisfy the INSTALL and check utilities, the  Imports: directive should include %s", impLine))
        invisible(pkgs)
    }
    else {
        message("Depends/Imports seem adequate.")
        invisible(dpdsOrImps)
    }
}

.codeRequiresFunctions <-
    c("::", ":::", "require", "library",
      "loadNamespace", "attachNamespace", "requireNamespace")

.codeRequiresDefs <-
    list(
       require = base::require,
       library = base::library,
       loadNamespace = base::loadNamespace,
       attachNamespace = base::attachNamespace,
       requireNamespace = base::requireNamespace)

.codeRequiresArg <- .codeRequiresDefs
for(i in names(.codeRequiresDefs))
    .codeRequiresArg[[i]] <-
      formalArgs(.codeRequiresDefs[[i]])[[1]]


## infer required packages from:  `::` or `:::` calls
##   and explicit calls to various attach or load functions
codeRequires <- function(file) {
    calls <- listOfCalls(file, .codeRequiresFunctions)
    if(length(calls) ==0)
        return(character())
    takesSymbolArg <- c("require", "library")
    whatFor <- character(length(calls))
    for(i in seq_along(calls)) {
        lli <- calls[[i]]
        what <- whatFor[[i]] <- as.character(lli[[1]])
        calls[[i]] <- switch(what,
               "::" = , ":::" = as.character(lli[[2]]),
                {
                    def <- .codeRequiresDefs[[what]]
                    if(is.null(def))
                        character() # unknown -- error?
                    else {
                        theCall <- match.call(def, lli, FALSE)
                        pkg <- theCall[[.codeRequiresArg[[what]] ]]
                        if(is.symbol(pkg) && what %in% takesSymbolArg)
                            as.character(pkg)
                        else if(is.character(pkg) && length(pkg) == 1 && nzchar(pkg))
                            pkg
                        else
                            lli # unknown, leave it alone
                    }
                })
    }
    ## eliminate empties or non-simple arguments
    ok <- sapply(calls, function(x) is.character(x) && length(x) == 1 && nzchar(x))
    unknown <- calls[!ok] # expressions we didn't understand
    attachNeeded <- !(whatFor %in% c("::", ":::", "loadNamespace"))
    depends <- unique(unlist(calls[ok & attachNeeded]))
    imports <- unique(unlist(calls[ok & !attachNeeded]))
    ## but only packages not also in the depends list
    imports <- imports[ !(imports %in% depends) ]
    list(Depends = depends, Imports = imports, Unknown = unknown)
}

## temporary junk for testing
if(FALSE) {
    require(utils)
    library(SoDA)
    if(runif(1) > .5)
        loadNamespace("Matrix")
    else
        loadNamespace("utils")

    attachNamespace("graphics")
    xxx <- "affygen"
    attachNamespace(xxx)
}


