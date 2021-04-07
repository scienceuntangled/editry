#' Execute editly
#'
#' @references <https://github.com/mifi/editly/>
#'
#' @param json string: path to json file or json string, as returned by [er_spec()]
#' @param fast logical: if `TRUE`, run editly in 'fast' (low quality/preview) mode
#' @param ... : if a json file is not being used, you can provide command line arguments as individual parameters
#'
#' @return 0 for success (invisibly)
#'
#' @examples
#' \dontrun{
#'   er_exec_wait()
#' }
#'
#' @export
er_exec_wait <- function(json, fast = FALSE, ...) {
    assert_that(is.flag(fast), !is.na(fast))
    er_add_ffmpeg_path() ## needed, because editly needs to find ffprobe and ffmpeg
    cli <- er_editly_exe()
    if (!missing(json) && !is.null(json)) {
        assert_that(is.string(json))
        if (!file.exists(json)) {
            ## is it a json string?
            ok <- tryCatch({ jsonlite::fromJSON(json); TRUE }, error = function(e) FALSE)
            if (!ok) stop("json should be a json string or path to json file")
            jsonfile <- tempfile(fileext = ".json5")
            cat(json, file = jsonfile, sep = "\n")
        } else {
            jsonfile <- json
        }
        invisible(noder::nr_exec_wait(args = c(cli, if (fast) "--fast", "--json", jsonfile)))
    } else {
        invisible(do.call(noder::nr_exec_wait, args = as.character(c(as.list(c(cli, if (fast) "--fast")), ...))))
    }
}
