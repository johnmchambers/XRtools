classSummary <- function (Class, complete = TRUE, propertiesAreCalled = "Slots")
{
    getDirectSupers <- function(ClassDef) {
        supers <- ClassDef@contains
        direct <- sapply(supers, function(inheritance)
                         inheritance@distance == 1
                         )
        names(supers)[direct]
    }

    if (isClassDef(Class)) {
        ClassDef <- Class
        Class <- ClassDef@className
    }
    else if (complete)
        ClassDef <- getClass(Class)
    else ClassDef <- getClassDef(Class)
    x <- ClassDef@slots
    if (length(x))
        cat("Slots: ",paste(names(x), dQuote(x), sep = " = ", collapse=", "), fill = TRUE)
    s <- getDirectSupers(ClassDef)
    if(length(s))
        cat("\nSuperclasses: ", paste(dQuote(s), sep = "", collapse = ", "),
            "\n")
    if (nzchar(pkg <- ClassDef@package) && pkg != ".GlobalEnv")
        cat("Package: \"", pkg, "\"", "\n", sep = "")
}
