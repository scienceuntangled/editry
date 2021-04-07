#' Create various sections of an editly spec
#'
#' @references <https://github.com/mifi/editly/>
#' @param type string: the layer type (e.g. "title" or "image")
#' @param duration numeric: clip duration in seconds
#' @param layers list: a list of `er_layer` objects
#' @param transition list: default transition spec
#' @param layer list: default layer spec
#' @param layer_type list: default layerType spec
#' @param fps numeric: frames per second
#' @param out_path string: the path of the video file to create
#' @param defaults er_defaults: as returned by [er_defaults()]
#' @param ... : other parameters
#'
#' @return A list with class `er_clip`, `er_layer`, etc
#'
#' @seealso [er_spec()]
#'
#' @export
er_layer <- function(type, ...) {
    out <- c(list(type = type), list(...))
    class(out) <- c("er_layer", class(out))
    out
}

#' @rdname er_layer
#' @export
er_clip <- function(duration, layers) {
    if (inherits(layers, "er_layer")) layers <- list(layers)
    out <- list(duration = duration, layers = layers)
    class(out) <- c("er_clip", class(out))
    out
}

#' @rdname er_layer
#' @export
er_defaults <- function(transition, layer, layer_type) {
    out <- list()
    if (!missing(transition) && !is.null(transition)) {
        out <- c(out, list(transition = transition))
    }
    if (!missing(layer) && !is.null(layer)) {
        out <- c(out, list(layer = layer))
    }
    if (!missing(layer_type) && !is.null(layer_type)) {
        out <- c(out, list(layer_type = layer_type))
    }
    class(out) <- c("er_defaults", class(out))
    out
}

#' @rdname er_layer
#' @export
er_header <- function(fps = 30, out_path, defaults) {
    out <- list(fps = fps, outPath = out_path)
    if (!missing(defaults) && !is.null(defaults)) out$defaults <- defaults
    class(out) <- c("er_header", class(out))
    out
}


#' Create the json string ready for compiling to video by editly
#'
#' @references <https://github.com/mifi/editly/>
#' @param header er_header: as returned by [er_header()]
#' @param clips list: a list of `er_clip` objects as returned by [er_clip()]
#'
#' @return A json string
#'
#' @seealso [er_layer()], [er_exec_wait()]
#'
#' @export
er_spec <- function(header, clips) {
    toJSON(c(header, clips = list(clips)), auto_unbox = TRUE)
}
