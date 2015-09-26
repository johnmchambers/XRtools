#' Character String for the Pointer to an R object
#'
#' Returns a character string containing the hexadecimal form of
#' the \dQuote{address} of an arbitrary R object.
#'@param x Any R object
#'@template authorKeyword
pointerString <- function(x)
    .Call(pointer_string, x)
