#' Install the editly node package
#'
#' @references <https://github.com/mifi/editly/>
#'
#' @param force logical: if `TRUE`, force reinstallation of editly if it is already installed
#' @param dependencies logical: if `TRUE`, the system binaries `node` (via [noder::nr_install_node()]) and `ffmpeg` (via [er_install_ffmpeg()]) will also be installed if not found on your system
#'
#' @return The path to the installation (invisibly)
#'
#' @export
er_install_editly <- function(force = FALSE, dependencies = TRUE) {
    if (isTRUE(dependencies) && is.null(noder::nr_node_exe())) {
        message("installing node")
        noder::nr_install_node()
    }
    out <- do_install_editry(force = force)
    if (isTRUE(dependencies) && is.null(er_ffmpeg_exe())) {
        message("installing ffmpeg")
        er_install_ffmpeg()
    }
    invisible(out)
}

do_install_editry <- function(force) {
    cwd <- getwd()
    on.exit(setwd(cwd))
    target_dir <- editry_app_dir()
    if (!is.null(er_editly_exe())) {
        if (!isTRUE(force)) {
            lver <- tryCatch(jsonlite::fromJSON(file.path(dirname(er_editly_exe()), "package.json"))$version, error = function(e) "unknown")
            rver <- tryCatch(jsonlite::fromJSON("https://raw.githubusercontent.com/scienceuntangled/editly/master/package.json")$version, error = function(e) "unknown")
            message("editly (version ", lver, ") already installed, use `force = TRUE` to force reinstallation. Remote version: ", rver)
            return(er_editly_exe())
        } else {
            ## remove current installation
#            try(unlink(target_dir, recursive = TRUE))
        }
    }
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
    setwd(target_dir)
    noder::nr_add_node_path()
    ##sys::exec_wait(Sys.which("npm"), c("i", "editly"))
    ## install from github
    message("installing editly to ", target_dir)
    res <- sys::exec_internal(Sys.which("npm"), c("i", "scienceuntangled/editly.git#logo"), error = FALSE)
    if (res$status > 0) {
        stop(rawToChar(res$stderr))
    } else {
        chk <- er_editly_exe()
        if (!is.null(chk)) chk else stop("Sorry, editly install failed")
    }
}

er_editly_exe <- function() {
    chk <- file.path(editry_app_dir(), "node_modules", "editly", "cli.js")
    if (file.exists(chk)) fs::path_real(chk) else NULL
}
