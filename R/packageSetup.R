packageSetup <- function(files = "tools/setup.R", dir = ".", needPackage = TRUE) {
    wd <- getwd()
    optT <- getOption("topLevelEnvironment")
    on.exit({setwd(wd); options(topLevelEnvironment = optT)})
    if(!missing(dir))
        setwd(dir)
    ## check that this is a package source directory
    if(!file.exists("DESCRIPTION"))
        stop(gettextf("directory (%s) should be a package source directory, but no DESCRIPTION file",
                      dQuote(getwd())))
    if(needPackage) {
        package <- read.dcf("DESCRIPTION")[,"Package"][[1]]
        env <- new.env(parent = asNamespace(package))
    }
    else env <- parent.frame()
    options(topLevelEnvironment = env) # metadata for classes, methods, goes here
    for(file in files)
        eval(parse(file),env)
}
