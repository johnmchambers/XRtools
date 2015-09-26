.getKnownEnvs <- function() {
    envs <- list("<environment:R_GlobalEnv>" = .GlobalEnv,
                 "<environment: base>" = baseenv(),
                 "<emptyenv>" = emptyenv())
    ss <- search()
    ss <- ss[grep("^package:", ss)]
    ss <- sapply(sub("package:","", ss), function(x)asNamespace(x))
    names(ss) <- paste0("<environment: namespace:", names(ss), ">")
    c(envs, ss)
}

#allow list of known environments to change, by putting it in an enviroment
.KnownEnvs <- new.env()
.KnownEnvs$envs <- .getKnownEnvs

envName <- function(envir) {
    atr <- attr(envir, "name")
    if(!is.null(atr))
        return(atr)
    evs <- .KnownEnvs$envs
    ## one of the attached package env's ?
        for(i in seq_along(evs))
            if(identical(envir, evs[[i]]))
                return( names(evs)[[i]])
    ## one of the active frames ?
        for(i in rev(seq_len(sys.parent())))
            if(identical(envir, sys.frame(i)))
                return(frameEnvName(i))
    ## next, some functions with special environments
    if(exists(".Generic", envir = envir, inherits = FALSE))
        return(paste0("<environment: generic function: ",
                      get(".Generic", envir =envir), ">"))
    ## an object from a reference class ?
    if(exists(".refClassDef", envir = envir, inherits = FALSE))
        return(paste0("<environment: object of class ",
                      shQuote(get(".refClassDef", envir = envir)@className),
                      ": ", pointerString(envir), ">"))
    ## a package namespace, not attached (NB: have to elminate Generic functions first)
    pkg <- utils::packageName(envir)
    if(!is.null(pkg)) # typically, a namespace loaded but not attached
            return(paste0("<environment: namespace: ",pkg))
    ## finally, produce the standard printed form, with the pointer
    paste0("<environment: ", pointerString(envir),">")
}

envNames <- function(envir = sys.frame(sys.parent())) {
    .KnownEnvs$envs <- .getKnownEnvs()
    emptyEnv <- emptyenv()
    enames <- character()
    while(!identical(envir, emptyEnv)) {
          enames <- c(enames, envName(envir))
          prev <- envir; envir <- parent.env(envir)
      }
    enames
}

frameEnvName <- function(which = sys.parent()) {
    call <- sys.call(which)
    paste0("<environment: frame ",which, ": ",
           if(is.name(call[[1]])) as.character(call[[1]])
           else "(function()..)", "()>")
}
