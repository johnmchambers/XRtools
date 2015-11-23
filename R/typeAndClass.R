typeAndClass <- function(..., labLen = getOption("labLen", 12)) {
    args <- substitute(list(...))
    labels <- character(nargs())
    for(i in seq_along(labels))
        labels[[i]] = deparse(args[[i+1]])[[1]]
    trunc <- nchar(labels) > labLen
    labels[trunc] <- paste0(substr(labels[trunc],1,labLen-2),"..")
    things <- eval(list(...))
    matrix(sapply(things, function(thing) c(class(thing)[[1]], typeof(thing))),
           nrow = 2, dimnames = list(c("Class", "Type"), labels))
}
