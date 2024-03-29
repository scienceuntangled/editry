.onAttach <- function(libname, pkgname) {
    ## some checks
    if (!nzchar(Sys.which("docker"))) {
        ## if docker exists, could be using editly via that, so don't warn
        node_installed <- requireNamespace("noder", quietly = TRUE) && !is.null(noder::nr_node_exe())
        editly_installed <- !is.null(er_editly_exe())
        needs_install <- c(if (!node_installed) "node", if (!editly_installed) "editly")
        if (length(needs_install) > 0) {
            msg <- c(paste0(paste(needs_install, collapse = " and "), " need", if (length(needs_install) == 1) "s", " to be installed before editry can be used. Run:"), "  er_install_editly()")
            packageStartupMessage(paste(msg, collapse = "\n"))
        }
    }
}
