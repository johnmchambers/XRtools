methodTags <- setClass("methodTags", slots = c(fdef = "function", fnames = "character",
                                     signature = "character"),
                       contains = "character")

setMethod("show", "methodTags", function(object) {
    f <- object@fdef@generic
    cat(gettextf("methodTags for function %s (package %s)\n",
                 dQuote(f), attr(f, "package")))
    cat("Target signature: "); cat(object@signature, sep = ", ", fill=TRUE)
    cat("\nMethods: ")
    fnames <- object@fnames
    if(length(fnames) == 0)
        cat("<NONE>\n")
    else if(all(fnames == f))
        cat(object@.Data, sep = ", ", fill = TRUE)
    else {
        mm = rbind(object@.Data, fnames)
        dimnames(mm) <- list(c("Tag: ","Function: "), rep("", length(fnames)))
        print(mm)
    }
})

eligibleMethods <- function(f, classes, doGroups = TRUE) {
    fdef <- getGeneric(f)
    methods <- findMethodSignatures(fdef)
    nargs <- ncol(methods)
    if(nargs > length(classes))
        stop(gettextf("Need  %d arguments from signature, got only %d",
                      nargs, length(classes)))
    else if(nargs < length(classes))
        classes <- classes[seq(length.out = nargs)]
    ## construct the tag strings for superclass combinations
    supers <- lapply(classes, extends)
    tags <- supers[[1]]
    for(i in seq(2, length.out = length(supers)-1))
        tags <- outer(tags, supers[[i]], function(x,y)paste(x, y, sep = "#"))
    tags <- as.vector(tags)
    methodTags <- names(findMethods(f))
    tags <- tags[ tags %in% methodTags ]
    fnames <- rep(fdef@generic, length(tags))
    if(doGroups && length(fdef@group)) {
        gptags <- Recall(fdef@group[[1]], classes, doGroups)
        fnames <- c(fnames, gptags@fnames)
        tags <- c(tags, as.character(gptags))
    }
    methodTags(tags, fnames = fnames, fdef = fdef, signature = classes)
}
