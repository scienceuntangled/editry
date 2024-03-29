## adapted from http://conjugateprior.org/2015/06/identifying-the-os-from-r/
get_os <- function() {
    if (.Platform$OS.type == "windows") return("windows")
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf["sysname"]
        if (tolower(os) == "darwin")
            os <- "osx"
    } else {
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os, ignore.case = TRUE))
            os <- "osx"
        if (grepl("linux-gnu", R.version$os, ignore.case = TRUE))
            os <- "linux"
    }
    os <- tolower(os)
    if (!os %in% c("windows", "linux", "unix", "osx"))
        stop("unknown operating system: ", os)
    os
}

nested_merge <- function(x, y) {
    ## merge anything from y into x, if there isn't already such an element in x
    ## recurse into list elements
    listels <- intersect(names(x)[vapply(x, is.list, FUN.VALUE = TRUE)], names(y)[vapply(y, is.list, FUN.VALUE = TRUE)])
    to_merge <- unique(c(setdiff(names(y), names(x)), listels))
    for (m in to_merge) {
        if (is.list(y[[m]])) {
            x[[m]] <- nested_merge(x[[m]], y[[m]])
        } else {
            x[[m]] <- y[[m]]
        }
    }
    x
}
