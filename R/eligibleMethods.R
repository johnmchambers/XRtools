methodTags <- setClass("methodTags", slots = c(fdef = "function", fnames = "character",
                                     signature = "character", package = "character"),
                       contains = "character")

setMethod("show", "methodTags", function(object) {
    f <- object@fdef@generic
    cat(gettextf("Method tags for function %s (package %s)\n",
                 dQuote(f), attr(f, "package")))
    notes <-character()
    if(length(object@signature) && !all(object@signature == "ANY"))
        notes <- paste("Target signature:", paste(object@signature, collapse = "#"))
    if(length(object@package))
        notes <- c(notes, paste("Methods from package:", object@package))
    if(length(notes))
        cat(notes, sep = "; ", fill=TRUE)
    fnames <- object@fnames
    if(length(fnames) == 0)
        cat("<NONE>\n")
    else if(all(fnames == f)) {
        print(object@.Data)
    }
    else {
        mm = rbind(object@.Data, fnames)
        dimnames(mm) <- list(c("Tag: ","Function: "), rep("", length(fnames)))
        print(mm)
    }
})

eligibleMethods <- function(f, classes = character(), package = character(), doGroups = TRUE) {
    fdef <- getGeneric(f)
    if(length(package))
        methods <- findMethods(f, asNamespace(package))
    else
        methods <- findMethods(f)
    methodSigs <- findMethodSignatures(methods = methods)
    nargs <- ncol(methodSigs)
    if(nargs > length(classes))
        classes <- c(classes, rep("ANY",
                      nargs - length(classes)))
    else if(nargs < length(classes))
        classes <- classes[seq(length.out = nargs)]
    ## construct the tag strings for superclass combinations
    allClasses <- unique(as.vector(methodSigs)) # so ANY will match.
    supers <- lapply(classes, function(class) if( class == "ANY") allClasses else c(extends(class), "ANY"))
    tags <- supers[[1]]
    for(i in seq(2, length.out = length(supers)-1))
        tags <- outer(tags, supers[[i]], function(x,y)paste(x, y, sep = "#"))
    tags <- as.vector(tags)
    methodTags <- names(methods)
    tags <- tags[ tags %in% methodTags ]
    fnames <- rep(fdef@generic, length(tags))
    if(doGroups && length(fdef@group)) {
        gptags <- Recall(fdef@group[[1]], classes, package, doGroups)
        fnames <- c(fnames, gptags@fnames)
        tags <- c(tags, as.character(gptags))
    }
    methodTags(tags, fnames = fnames, fdef = fdef, signature = classes, package = package)
}
