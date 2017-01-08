debugInstall <- function(...) {
    cmdArgs <- unlist(list(...))
    environment(.install_packages) <- asNamespace("tools")
    .install_packages(cmdArgs)
}
