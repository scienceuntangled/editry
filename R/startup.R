.onAttach <- function(libname, pkgname) {
    ## some checks
    node_installed <- !is.null(nr_node_exe())
    editly_installed <- !is.null(er_editly_exe())
    needs_install <- c(if (!node_installed) "node", if (!editly_installed) "editly")
    if (length(needs_install) > 0) {
        msg <- paste0(paste(needs_install, collapse = " and "), " need", if (length(needs_install) == 1) "s", " to be installed before editry can be used.")
        if (!node_installed) {
            if (get_os() %in% c("osx", "linux") && !requireNamespace("archive", quietly = TRUE)) {
                msg <- c(msg, "the 'archive' package is required to install node: first install 'archive' with")
                if (!requireNamespace("remotes", quietly = TRUE)) msg <- c(msg, "  install.packages(\"remotes\")")
                msg <- c(msg, "  remotes::install_github(\"jimhester/archive\")", "then run:")
            }
        } else {
            msg <- c(msg, "Run:")
        }
        msg <- c(msg, "  er_install_editly()")
        packageStartupMessage(paste(msg, collapse = "\n"))
    }
}
