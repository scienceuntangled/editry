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
    if (!requireNamespace("noder", quietly = TRUE)) stop("the noder package is required, install with `er_install_editly()`")
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
        invisible(noder::nr_exec_wait(args = c(cli, if (isTRUE(fast)) "--fast", "--json", jsonfile)))
    } else {
        invisible(do.call(noder::nr_exec_wait, args = as.character(c(as.list(c(cli, if (isTRUE(fast)) "--fast")), ...))))
    }
}


#' Execute editly in docker image
#'
#' Does not require node or editly to be installed on your system, instead requires that Docker is running and that you have pulled a suitable editly docker image. By default this is `scienceuntangled/editly`, which uses a modified version of `editly`. Other `editly` docker images can be used, but will not support the `logo_*` parameters in `er_header` or `er_spec`.
#'
#' @references <https://github.com/mifi/editly/>
#'
#' @param spec er_spec: an `er_spec` object as returned by [er_spec()]
#' @param docker_image string: name of the docker image to use
#' @param fast logical: if `TRUE`, run editly in 'fast' (low quality/preview) mode
#'
#' @return 0 for success (invisibly)
#'
#' @examples
#' \dontrun{
#'   clips <- list(er_clip_title2(duration = 3, text = "Title slide"),
#'                 er_clip_image(duration = 3, path = "https://jeroen.github.io/images/Rlogo.png"))
#'   outfile <- tempfile(fileext = ".mp4")
#'   my_spec <- er_spec(clips = clips , out_path = outfile, allow_remote_requests = TRUE)
#'   er_exec_docker(my_spec)
#'   if (interactive()) browseURL(outfile)
#' }
#'
#' @export
er_exec_docker <- function(spec, docker_image = "scienceuntangled/editly", fast = FALSE) {
    v <- system.file("extdata", package = "editry")
    assert_that(inherits(spec, "er_spec"), msg = "spec should be an er_spec object")
    ## find paths to map to docker
    temp <- unlist(spec)
    for (i in seq_along(temp)) {
        this <- strsplit(names(temp)[i], "\\.")[[1]]
        if (any(this %in% c("path", "audioFilePath", "fontPath", "logoPath", "outPath")) && !grepl("^https?:", temp[i], ignore.case = TRUE)) v <- c(v, dirname(temp[i]))
    }
    jsonfile <- tempfile(fileext = ".json5")
    v <- c(v, dirname(jsonfile))
    cat(er_to_json(spec), file = jsonfile, sep = "\n")
    fargs <- c("--json", jsonfile)
    ## TODO consolidate v to a reduced set of (higher) common paths?
    v2 <- vapply(unique(v), function(thisv) c("-v", paste0(thisv, ":", thisv)), FUN.VALUE = character(2), USE.NAMES = FALSE)
    fargs <- c("run", "--rm", v2, docker_image, "editly", if (isTRUE(fast)) "--fast", fargs)
    ## also pass user and id? would only work on unix-alikes? docker run --rm -u $(id -u):$(id -g) ...
    ##print(fargs)
    invisible(sys::exec_wait(cmd = "docker", fargs))
}
