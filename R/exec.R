#' Execute editly
#'
#' @references <https://github.com/mifi/editly/>
#'
#' @param spec er_spec or string: `er_spec` object, or a string giving the path to a JSON file or a JSON string, as returned by [er_spec()]
#' @param fast logical: if `TRUE`, run editly in 'fast' (low quality/preview) mode
#' @param ... : if a JSON file is not being used, you can provide command line arguments as individual parameters
#'
#' @return 0 for success (invisibly)
#'
#' @examples
#' \dontrun{
#'   clips <- list(er_clip_title2(duration = 3, text = "Title slide"),
#'                 er_clip_image(duration = 3, path = "https://jeroen.github.io/images/Rlogo.png"))
#'   outfile <- tempfile(fileext = ".mp4")
#'   my_spec <- er_spec(clips = clips , out_path = outfile, allow_remote_requests = TRUE)
#'   er_exec_wait(my_spec)
#'   if (interactive()) browseURL(outfile)
#' }
#'
#' @export
er_exec_wait <- function(spec, fast = FALSE, ...) {
    assert_that(is.flag(fast), !is.na(fast))
    er_add_ffmpeg_path() ## needed, because editly needs to find ffprobe and ffmpeg. Also checks that ffmpeg can be found (is installed)
    cli <- er_editly_exe()
    if (is.null(cli)) stop("could not find the editly executable: have you installed it with `er_install_editly()` ?")
    if (!missing(spec) && !is.null(spec)) {
        assert_that(is.string(spec) || inherits(spec, "er_spec"), msg = "spec should be an er_spec object or a string")
        if (inherits(spec, "er_spec")) spec <- er_to_json(spec)
        assert_that(is.string(spec))
        if (!file.exists(spec)) {
            ## is it a JSON string?
            ok <- tryCatch({ jsonlite::fromJSON(spec); TRUE }, error = function(e) FALSE)
            if (!ok) stop("spec should be a JSON string or path to JSON file")
            jsonfile <- tempfile(fileext = ".json5")
            cat(spec, file = jsonfile, sep = "\n")
        } else {
            jsonfile <- spec
        }
        invisible(noder::nr_exec_wait(args = c(cli, if (fast) "--fast", "--json", jsonfile)))
    } else {
        invisible(do.call(noder::nr_exec_wait, args = as.character(c(as.list(c(cli, if (fast) "--fast")), ...))))
    }
}
