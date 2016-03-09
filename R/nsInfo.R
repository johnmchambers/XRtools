.asNS <- function(ns) {
    if(is.character(ns))
        asNamespace(ns)
    else if(is(ns, "environment"))
        ns
    else
        stop(gettextf("Expected the name of a package or a namespace; got an object of class %s",
                      dQuote(class(ns))), call. = FALSE)
}


nsImports <- function(ns, all = FALSE, includeBase = FALSE) {
    ns <-.asNS(ns)
    info <- getNamespaceInfo(ns, "imports")
    if(!includeBase)
        info$base <- NULL
    if(all)
        info
    else
        names(info)
}
