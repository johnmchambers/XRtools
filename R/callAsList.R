.toListW  <- makeCodeWalker(
    call = function(e, w)
        lapply(e, function(ee) walkCode(ee, w)),
    leaf = function(e, w)
        e
    )

callAsList <- function(expr)
    walkCode(expr, .toListW)

