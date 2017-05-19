## dummy version to avoid including C code
pointerString <- function(x) {
    ## .Call(pointer_string, x)
    name <- attr(x, "name")
    if(is.null(name))
        ""
    else
        name
}
