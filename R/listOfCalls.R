listOfCalls <- function(expr, functions)
    list()

setGeneric("listOfCalls")

setMethod("listOfCalls", "language",
    function(expr, functions) {
        expr <- as(expr, "list")
        thisValue <-
            if(as(expr[[1]], "character") %in% functions)
                list(expr)
            else
                thisValue <- list()
        others <-
            lapply(expr[-1], listOfCalls,
                   functions)
        c(thisValue, unlist(others, FALSE))
    })

setMethod("listOfCalls", "name",
          function(expr, functions)
              list())

setMethod("listOfCalls", "expression",
          function(expr, functions)
            unlist(lapply(as(expr, "list"),
               listOfCalls, functions), FALSE))
