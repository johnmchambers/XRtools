betterCitation <- function(pkg, outfile = stdout()) {
    ctn <- utils::citation(pkg)
    txt <- strsplit(format(ctn, "Bibtex")," *\n")[[1]]
    txt[[1]] <- paste0("@Manual{",pkg,",")
    n1 <- grep("note  *= *",txt)
    note <- txt[n1]
    note <- gsub("},$","",note)
    txt <- txt[-n1]
    n <- grep("url  *= *",txt)
    url <- txt[n]
    url <- gsub(" *url  *= *", "",url)
    url <- paste0(note, " \\url", url)
    url <- gsub(",*$","},",url)
    txt[n] <- url
    writeLines(txt, outfile)
    invisible(txt)
}
