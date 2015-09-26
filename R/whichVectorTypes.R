whichSubclasses <- function(Class, distance = NA, simple = NA) {
    .whichOthers(Class, distance, simple, sub = TRUE)
}

whichSuperclasses <- function(Class, distance = NA, simple = NA) {
    .whichOthers(Class, distance, simple, sub = FALSE)
}

.whichOthers <- function(Class, distance, simple, sub) {
    .inRange <- function(x) x >= xlow && x <= xhigh
    xlow <- min(distance)
    xhigh <- max(distance)
    if(is(Class, "character"))
        classDef <- getClass(Class)
    else
        classDef <- as(Class, "classRepresentation")
    relations <- if(sub) classDef@subclasses else classDef@contains
    which <- sapply(relations, function(rel)
                 (is.na(distance) || .inRange(rel@distance)) &&
                 (is.na(simple) || identical(rel@simple, simple))
                    )
    names(which)[which]
}
showVectorTypes <- function(withClasses = FALSE) {
    classes <- whichSubclasses("vector", distance = 1, simple = TRUE)
    types <- sapply(classes, function(x) typeof(new(x)))
    if(withClasses)
        types
    else
        as.vector(types)
}
