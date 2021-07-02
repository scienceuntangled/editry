#' Install ffmpeg
#'
#' This is a helper function to install ffmpeg. Currently it only works on Windows and Linux platforms. The ffmpeg bundle will be downloaded from <https://github.com/BtbN/FFmpeg-Builds/releases/latest> (Windows) or <https://johnvansickle.com/ffmpeg/> (Linux) and saved to your user appdata directory.
#'
#' @references <https://github.com/BtbN/FFmpeg-Builds/releases/latest> <https://johnvansickle.com/ffmpeg/>
#' @param force logical: force reinstallation if ffmpeg already exists
#' @param bits integer: 32 or 64, for 32- or 64-bit install. If missing or `NULL`, will be guessed based on `.Machine$sizeof.pointer`. Note that only 64-bit is supported on Windows
#' @param check_hash logical: don't check the hash of the downloaded file. Ignored on windows
#'
#' @return the path to the installed executable
#'
#' @examples
#' \dontrun{
#'   er_install_ffmpeg()
#' }
#'
#' @export
er_install_ffmpeg <- function(force = FALSE, bits, check_hash = TRUE) {
    assert_that(is.flag(force), !is.na(force))
    assert_that(is.flag(check_hash), !is.na(check_hash))
    if (missing(bits) || is.null(bits)) {
        bits <- tryCatch(if (.Machine$sizeof.pointer==8) 64 else 32, error = function(e) 64)
    }
    assert_that(bits %in% c(32, 64))
    my_os <- get_os()
    if (!((my_os == "windows" && bits == 64) || my_os == "linux")) {
        stop("er_install_ffmpeg only supports linux or 64-bit windows platforms. You will need to install ffmpeg yourself and ensure that it is on the system path.")
    }
    existing_exe <- er_ffmpeg_exe()
    path <- file.path(ovideo_app_dir(), "ffmpeg")
    if (!force) {
        if (!is.null(existing_exe)) {
            message("ffmpeg already exists and force is FALSE, not reinstalling")
            return(existing_exe)
        }
    } else {
        if (dir.exists(path)) {
            ## remove existing
            unlink(path, recursive = TRUE)
        }
    }
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    if (!dir.exists(path)) stop("could not create directory ", path, " for ffmpeg")

    if (my_os == "windows") {
        ## e.g. https://github.com/BtbN/FFmpeg-Builds/releases/download/autobuild-2021-04-09-12-38/ffmpeg-N-101901-gb593abda6c-win64-gpl.zip
        pg <- readLines("https://github.com/BtbN/FFmpeg-Builds/releases/latest")
        pg <- pg[grep("\\/ffmpeg\\-N.+\\-win64\\-gpl.zip\"", pg)]
        rgxp <- paste0("href[[:space:]]*=[[:space:]]*\"(.+?)\"")
        dl_url <- pg[grep(rgxp, pg, ignore.case = TRUE)]
        dl_url <- regmatches(dl_url, regexpr(rgxp, dl_url, ignore.case = TRUE))
        if (length(dl_url) == 1) {
            dl_url <- paste0("https://github.com", gsub("\"", "", sub("href[[:space:]]*=[[:space:]]*\"", "", dl_url)))
        } else {
            stop("could not find download URL on https://github.com/BtbN/FFmpeg-Builds/releases/latest")
        }
        hash_url <- NULL
        ## alternative, requires archive to extract 7z
        ##dl_url <- "https://www.gyan.dev/ffmpeg/builds/ffmpeg-git-essentials.7z"
        ##hash_url <- "https://www.gyan.dev/ffmpeg/builds/sha256-git-essentials"
        ##hash_algo <- "sha256"
    } else if (my_os == "linux") {
        dl_url <- if (bits == 64) "https://johnvansickle.com/ffmpeg/builds/ffmpeg-git-amd64-static.tar.xz" else "https://johnvansickle.com/ffmpeg/builds/ffmpeg-git-i686-static.tar.xz"
        hash_url <- paste0(dl_url, ".md5")
        hash_algo <- "md5"
    } else {
        ## should not get here, but just in case
        stop("unsupported os")
    }
    zipname <- file.path(path, basename(dl_url))
    err <- utils::download.file(dl_url, destfile = zipname, mode = "wb")
    if (!err) {
        if (!is.null(hash_url) && check_hash) {
            expected_hash <- tryCatch(readLines(hash_url, warn = FALSE), error = function(e) NULL)
            if (is.null(expected_hash)) {
                warning("could not download the file hash for checking, but proceeding anyway")
            } else {
                if (grepl("[[:space:]]", expected_hash)) expected_hash <- sub("[[:space:]].*", "", expected_hash)
                if (!identical(expected_hash, digest::digest(zipname, algo = hash_algo, file = TRUE)))
                    stop("the hash of the downloaded file does not match the expected value. The downloaded file might be incomplete or compromised. If you wish to proceed anyway, run this function again with `check_hash = FALSE`")
            }
        }
        if (grepl("\\.zip$", zipname, ignore.case = TRUE)) {
            utils::unzip(zipname, exdir = path)
        } else {
            archive::archive_extract(zipname, dir = path)
        }
    }
    ## now we should see the executable
    chk <- er_ffmpeg_exe()
    if (!is.null(chk)) chk else stop("Sorry, ffmpeg install failed. You will need to install it yourself and ensure that it is on the system path.")
}


#' Path to the ffmpeg executable
#'
#' @return The path to the executable, or `NULL` if not found
#'
#' @seealso [er_install_ffmpeg()]
#'
#' @examples
#' er_ffmpeg_exe()
#'
#' @export
er_ffmpeg_exe <- function() {
    exe_name <- paste0("ffmpeg", if (get_os() == "windows") ".exe")
    chk <- Sys.which(exe_name)
    if (nzchar(chk)) return(chk)
    ## is it installed in user appdir?
    mydir <- file.path(ovideo_app_dir(), "ffmpeg")
    if (!dir.exists(mydir)) return(NULL)
    chk <- fs::dir_ls(path = mydir, recurse = TRUE, regexp = paste0(exe_name, "$"), type = "file")
    chk <- chk[basename(chk) == exe_name]
    if (length(chk) == 1 && file.exists(chk)) chk else NULL
}

#' Add the path to ffmpeg to the system path
#'
#' If `ffmpeg` can already be seen by the system, the system path will not be changed. Otherwise, the system path will be prepended with the ffmpeg path.
#'
#' @return `TRUE` on success, invisibly
#'
#' @export
er_add_ffmpeg_path <- function() {
    if (!nzchar(Sys.which("ffmpeg"))) {
        ## not already on the system path
        er_ffmpeg_ok(do_error = TRUE) ## check that we can find it
        ffmpeg_dir <- dirname(er_ffmpeg_exe())
        if (nzchar(ffmpeg_dir)) {
            psep <- if (get_os() == "windows") ";" else ":"
            Sys.setenv(PATH = paste0(ffmpeg_dir, psep, Sys.getenv("PATH")))
        }
    }
    if (!nzchar(Sys.which("ffmpeg"))) stop("could not set the path to ffmpeg")
    invisible(TRUE)
}

er_ffmpeg_ok <- function(do_error = FALSE) {
    exe <- er_ffmpeg_exe()
    ok <- !is.null(exe) && tryCatch(sys::exec_internal(exe, "-version")$status == 0, error = function(e) FALSE)
    if (!ok && do_error) stop("could not find the ffmpeg executable")
    invisible(ok)
}
