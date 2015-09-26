isPrimitiveGeneric <- function(what) {
    if(is.character(what))
        obj <- getFunction(what, mustFind = FALSE, where =asNamespace("base"))
    else {
        obj <- what
        what <- as.character(substitute(what))
    }
    if(!is.primitive(obj))
        return(FALSE)
    ## a few special cases that either hang or give false negative
    dontCall <- list(browser = FALSE, "repeat" = FALSE, "$<-" = TRUE)
    if(what %in% names(dontCall))
        return(dontCall[[what]])
    Class <- "TestPrimitiveClass"
    Method <- paste(what, Class, sep = ".")
    assign(Method,
           function(...)"Yes it is!",
           envir = .GlobalEnv)
    on.exit(rm(list = Method, envir = .GlobalEnv))
    arg <- NA
    class(arg) <- Class
    if(grepl("<-$", what)) { # will need a value= argument
        test1 <- substitute(tryCatch(FUN(arg,  value = 1), error = function(e)e),
                       list(FUN = as.name(what)))
        test2 <- substitute(tryCatch(FUN(arg, arg, value = 1), error = function(e)e),
                       list(FUN = as.name(what)))
    }
    else {
        test1 <- substitute(tryCatch(FUN(arg), error = function(e)e),
                       list(FUN = as.name(what)))
        test2 <- substitute(tryCatch(FUN(arg, arg), error = function(e)e),
                       list(FUN = as.name(what)))
    }
    value <- eval(test1)
    if(is(value, "error")) # well, maybe it would work with 2 args
        value <- eval(test2)
    identical(value, "Yes it is!")
}
