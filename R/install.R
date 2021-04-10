#' Install the editly node package
#'
#' @references <https://github.com/mifi/editly/>
#'
#' @return The path to the installation
#'
#' @export
er_install_editly <- function() {
    target_dir <- editry_app_dir()
    if (!is.null(er_editly_exe())) {
        message("editly already installed")
        return(er_editly_exe())
    }
    cwd <- getwd()
    on.exit(setwd(cwd))
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
    setwd(target_dir)
    noder::nr_add_node_path()
    ##sys::exec_wait(Sys.which("npm"), c("i", "editly"))
    ## install from github
    sys::exec_wait(Sys.which("npm"), c("i", "scienceuntangled/editly.git#logo"))
    chk <- er_editly_exe()
    if (!is.null(chk)) chk else stop("Sorry, editly install failed")
}


er_editly_exe <- function() {
    chk <- file.path(editry_app_dir(), "node_modules", "editly", "cli.js")
    if (file.exists(chk)) fs::path_real(chk) else NULL
}
