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
    if (nzchar(pkg <- ClassDef@package) && pkg != ".GlobalEnv")
        cat("Package: \"", pkg, "\"", "\n", sep = "")
    x <- ClassDef@slots
    if (length(x)) {
        cat("Slots:\n ")
        print(matrix(x, nrow = 1, byrow=TRUE, dimnames = list("Class", names(x))))
    }
    s <- getDirectSupers(ClassDef)
    if(length(x))
        cat("\nSuperclasses: ", paste('"', s, '"', sep = "", collapse = ", "),
            "\n")

}
