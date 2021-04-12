## turn path into path_real, but not for URLs
cpr <- function(z) if (!grepl("^https?:", z, ignore.case = TRUE)) fs::path_real(z) else z

## conditionally add thing to addto
cadd <- function(addto, thing) {
    if (!missing(thing) && length(thing) > 0) {
        nm <- as.character(substitute(thing))
        is_path <- is.string(thing) && (nm == "path" || grepl("_path", nm))
        ## underscore_names to camelCase
        nm <- R.utils::toCamelCase(nm, split = "_")
        addto[[nm]] <- if (is_path) cpr(thing) else thing
    }
    addto
}

vok <- function(z) !missing(z) && !is.null(z)

## filter out NULL elements (from list)
nnull <- function(z) Filter(Negate(is.null), z)

#' Create various sections of an editly spec
#'
#' @references <https://github.com/mifi/editly/>
#' @param type string: the layer type (e.g. "title", "image", "video")
#' @param duration numeric: duration in seconds
#' @param layers list: a list of `er_layer` objects
#' @param transition list: an [er_transition()] object
#' @param layer list: default layer spec
#' @param layer_type list: default layerType spec
#' @param fps numeric: frames per second (default = 25 or FPS of first video)
#' @param width integer: video width (default = 640)
#' @param height integer: auto based on width and aspect ratio of first video
#' @param allow_remote_requests logical: allow remote URLs as paths? (default = `FALSE`)
#' @param out_path string: the path of the video file to create (the file extension can be a video file extension or ".gif")
#' @param audio_file_path string: set an audio track for the whole video
#' @param loop_audio logical: loop the audio track if it is shorter than video? (default = `FALSE`)
#' @param keep_source_audio logical: keep source audio from clips? (default = `FALSE`)
#' @param clips_audio_volume numeric: volume of audio from clips relative to [er_audio_track()]s (default = 1)
#' @param output_volume numeric or string: adjust output volume (final stage). e.g. 0.5 or "10dB" (default = 1)
#' @param audio_norm.enable logical: enable audio normalization? (default = `FALSE`)
#' @param audio_norm.gauss_size numeric: audio normalization gauss size (default = 5)
#' @param audio_norm.max_gain numeric: audio normalization max gain (default = 30)
#' @param custom_output_args character: vector of custom output codec/format arguments for ffmpeg
#' @param logo_path string: path to logo file. This must be a local file, even if `allow_remote_requests` is `TRUE`. `logo_path` will be ignored if `out_path` is a gif
#' @param logo_width numeric: width of logo relative to screen width (0 to 1, default = 0.2). If only one of logo height or width is specified, the logo aspect ratio will be retained
#' @param logo_height numeric: height of logo relative to screen width (0 to 1, default = 0.1). If only one of logo height or width is specified, the logo aspect ratio will be retained
#' @param logo_x numeric: logo left-hand-edge position relative to left of screen (0 = left to 1 = right, default = 0.78). The logo is referenced relative to its left-hand edge, so the default of 0.78 and width 0.2 puts the right-hand edge at 0.98
#' @param logo_y numeric: logo bottom-edge position relative to top of screen (0 = top to 1 = bottom, default = 0.98)
#' @param defaults er_defaults: as returned by [er_defaults()]
#' @param ... : other parameters
#'
#' @return A list with class `er_clip`, `er_layer`, etc
#'
#' @seealso [er_spec()]
#'
#' @export
er_layer <- function(type, ...) {
    rgs <- list(...)
    names(rgs) <- R.utils::toCamelCase(names(rgs), split = "_")
    out <- c(list(type = type), rgs)
    structure(out, class = c("er_layer", "list"))
}


#' @rdname er_layer
#' @export
er_clip <- function(duration, transition, layers) {
    if (inherits(layers, "er_layer")) layers <- list(layers)
    out <- list()
    out <- cadd(out, transition)
    out <- cadd(out, duration)
    out$layers <- layers
    structure(out, class = c("er_clip", "list"))
}

#' @rdname er_layer
#' @export
er_defaults <- function(transition, layer, layer_type) {
    out <- list()
    if (vok(transition)) out <- c(out, list(transition = transition))
    if (vok(layer)) out <- c(out, list(layer = layer))
    if (vok(layer_type)) out <- c(out, list(layerType = layer_type))
    structure(out, class = c("er_defaults", "list"))
}

#' @rdname er_layer
#' @export
er_header <- function(out_path = tempfile(fileext = ".mp4"), fps, width, height, allow_remote_requests, audio_file_path, loop_audio, keep_source_audio, clips_audio_volume, output_volume, audio_norm.enable, audio_norm.gauss_size, audio_norm.max_gain, defaults, custom_output_args, logo_path, logo_width, logo_height, logo_x, logo_y) {
    out <- list(outPath = out_path)
    out <- cadd(out, fps)
    out <- cadd(out, width)
    out <- cadd(out, height)
    out <- cadd(out, allow_remote_requests)
    out <- cadd(out, audio_file_path)
    out <- cadd(out, loop_audio)
    out <- cadd(out, keep_source_audio)
    out <- cadd(out, clips_audio_volume)
    out <- cadd(out, output_volume)
    out <- cadd(out, audio_norm.enable)
    out <- cadd(out, audio_norm.gauss_size)
    out <- cadd(out, audio_norm.max_gain)
    out <- cadd(out, custom_output_args)
    out <- cadd(out, defaults)
    out <- cadd(out, logo_path)
    out <- cadd(out, logo_width)
    out <- cadd(out, logo_height)
    out <- cadd(out, logo_x)
    out <- cadd(out, logo_y)
    structure(out, class = c("er_header", "list"))
}



## convenience definitions for layer types

#' Standard layer types
#'
#' position can be one of either:
#' * top, bottom center, top-left, top-right, center-left, center-right, bottom-left, bottom-right
#' * a list object {x, y, originX = 'left', originY = 'top' }, where { x: 0, y: 0 } is the upper left corner of the screen, and { x: 1, y: 1 } is the lower right corner, x is relative to video width, y to video height. originX and originY are optional, and specify the position's origin (anchor position) of the object.
#'
#' Audio layers will be mixed together. If cutFrom/cutTo is set, the resulting segment (cutTo-cutFrom) will be slowed/sped-up to fit clip.duration. The slow down/speed-up operation is limited to values between 0.5x and 100x.
#' `er_layer_detached_audio` is a special case of `audio_tracks` that makes it easier to start the audio relative to clips start times without having to calculate global start times. detached audio has the exact same properties as `audio_tracks`, except start time is relative to the clip's start.
#'
#' For video layers, if parent clip.duration is specified, the video will be slowed/sped-up to match clip.duration. If cutFrom/cutTo is set, the resulting segment (cutTo-cutFrom) will be slowed/sped-up to fit clip.duration. If the layer has audio, it will be kept (and mixed with other audio layers if present.)
#'
#' resize_mode - How to fit image to screen. Can be one of:
#' * "contain" - All the video will be contained within the frame and letterboxed
#' * "contain-blur" - Like contain, but with a blurred copy as the letterbox
#' * "cover" - Video be cropped to cover the whole screen (aspect ratio preserved)
#' * "stretch" - Video will be stretched to cover the whole screen (aspect ratio ignored).
#' Default is "contain-blur".
#'
#' @references <https://github.com/mifi/editly/>
#' @param color string: slide color (default = random)
#' @param colors character: vector of two colors (default = random)
#' @param text string: text to show
#' @param font_path string: path to font
#' @param text_color string: text color
#' @param position string or list: see Details
#' @param zoom_direction string: "in", out", or "null" to disable
#' @param zoom_amount numeric: amount to zoom (default = 0.1)
#' @param path string: path to image or video file
#' @param width numeric: width (from 0 to 1) where 1 is screen width
#' @param height numeric: height (from 0 to 1) where 1 is screen height
#' @param resize_mode : see Details
#' @param cut_from numeric: time value to cut from, in seconds
#' @param cut_to numeric: time value to cut to, in seconds
#' @param left numeric: X-position relative to screen width (0 to 1, default = 0)
#' @param top numeric: Y-position relative to screen height (0 to 1, default = 0)
#' @param origin_x numeric: X anchor ("left" or "right", default = "left")
#' @param origin_y numeric: Y anchor ("top" or "bottom", default = "top")
#' @param mix_volume numeric: relative volume when mixing this video's audio track with others (default = 1)
#' @param background er_layer: as returned by [er_layer_radial_gradient()], [er_layer_linear_gradient()], or [er_layer_fill_color()]
#' @param background_color string: background color
#' @param font_size numeric: font size
#' @param char_spacing numeric: character spacing
#' @param start numeric: how many seconds into this clip should this audio track start? (default = 0)
#'
#' @return A list with class `er_layer`
#'
#' @seealso [er_layer()]
#'
#' @export
er_layer_pause <- function(color) {
    do.call(er_layer, nnull(list(type = "pause",
                                 color = if (!missing(color)) color)))
}

#' @rdname er_layer_pause
#' @export
er_layer_image <- function(path, resize_mode, zoom_direction, zoom_amount) {
    do.call(er_layer, nnull(list(type = "image",
                                 path = if (!missing(path)) cpr(path),
                                 resize_mode = if (!missing(resize_mode)) resize_mode,
                                 zoom_direction = if (!missing(zoom_direction)) zoom_direction,
                                 zoom_amount = if (!missing(zoom_amount)) zoom_amount)))
}

#' @rdname er_layer_pause
#' @export
er_layer_image_overlay <- function(path, position, width, height, zoom_direction, zoom_amount) {
    do.call(er_layer, nnull(list(type = "image-overlay",
                                 path = if (!missing(path)) cpr(path),
                                 position = if (!missing(position)) position,
                                 width = if (!missing(width)) width,
                                 height = if (!missing(height)) height,
                                 zoom_direction = if (!missing(zoom_direction)) zoom_direction,
                                 zoom_amount = if (!missing(zoom_amount)) zoom_amount)))
}

#' @rdname er_layer_pause
#' @export
er_layer_title <- function(text, font_path, text_color, position, zoom_direction, zoom_amount) {
    do.call(er_layer, nnull(list(type = "title",
                                 text = if (!missing(text)) text,
                                 font_path = if (!missing(font_path)) cpr(font_path),
                                 text_color = if (!missing(text_color)) text_color,
                                 position = if (!missing(position)) position,
                                 zoom_direction = if (!missing(zoom_direction)) zoom_direction,
                                 zoom_amount = if (!missing(zoom_amount)) zoom_amount)))
}

#' @rdname er_layer_pause
#' @export
er_layer_subtitle <- function(text, font_path, text_color) {
    do.call(er_layer, nnull(list(type = "subtitle",
                                 text = if (!missing(text)) text,
                                 font_path = if (!missing(font_path)) cpr(font_path),
                                 text_color = if (!missing(text_color)) text_color)))
}

#' @rdname er_layer_pause
#' @export
er_layer_title_background <- function(text, font_path, text_color, background) {
    do.call(er_layer, nnull(list(type = "title-background",
                                 text = if (!missing(text)) text,
                                 font_path = if (!missing(font_path)) cpr(font_path),
                                 text_color = if (!missing(text_color)) text_color,
                                 background = if (!missing(background)) background)))
}

#' @rdname er_layer_pause
#' @export
er_layer_news_title <- function(text, font_path, text_color, background_color, position) {
    do.call(er_layer, nnull(list(type = "news-title",
                                 text = if (!missing(text)) text,
                                 font_path = if (!missing(font_path)) cpr(font_path),
                                 text_color = if (!missing(text_color)) text_color,
                                 background_color = if (!missing(background_color)) background_color,
                                 position = if (!missing(position)) position)))
}


#' @rdname er_layer_pause
#' @export
er_layer_slide_in_text <- function(text, font_path, font_size, char_spacing, color, position) {
    do.call(er_layer, nnull(list(type = "slide-in-text",
                                 text = if (!missing(text)) text,
                                 font_path = if (!missing(font_path)) cpr(font_path),
                                 font_size = if (!missing(font_size)) font_size,
                                 char_spacing = if (!missing(char_spacing)) char_spacing,
                                 color = if (!missing(color)) color,
                                 position = if (!missing(position)) position)))
}

#' @rdname er_layer_pause
#' @export
er_layer_fill_color <- function(color) {
    do.call(er_layer, nnull(list(type = "fill-color",
                                 color = if (!missing(color)) color)))
}

#' @rdname er_layer_pause
#' @export
er_layer_radial_gradient <- function(colors) {
    do.call(er_layer, nnull(list(type = "radial-gradient",
                                 colors = if (!missing(colors)) colors)))
}

#' @rdname er_layer_pause
#' @export
er_layer_linear_gradient <- function(colors) {
    do.call(er_layer, nnull(list(type = "linear-gradient",
                                 colors = if (!missing(colors)) colors)))
}

#' @rdname er_layer_pause
#' @export
er_layer_rainbow_colors <- function() {
    er_layer(type = "rainbow-colors")
}

#' @rdname er_layer_pause
#' @export
er_layer_video <- function(path, resize_mode, cut_from, cut_to, width, height, left, top, origin_x, origin_y, mix_volume) {
    do.call(er_layer, nnull(list(type = "video",
                                 path = if (!missing(path)) cpr(path),
                                 resize_mode = if (!missing(resize_mode)) resize_mode,
                                 cut_from = if (!missing(cut_from)) cut_from,
                                 cut_to = if (!missing(cut_to)) cut_to,
                                 width = if (!missing(width)) width,
                                 height = if (!missing(height)) height,
                                 left = if (!missing(left)) left,
                                 top = if (!missing(top)) top,
                                 origin_x = if (!missing(origin_x)) origin_x,
                                 origin_y = if (!missing(origin_y)) origin_y,
                                 mix_volume = if (!missing(mix_volume)) mix_volume)))
}

#' @rdname er_layer_pause
#' @export
er_layer_audio <- function(path, cut_from, cut_to, mix_volume) {
    do.call(er_layer, nnull(list(type = "audio",
                                 path = if (!missing(path)) cpr(path),
                                 cut_from = if (!missing(cut_from)) cut_from,
                                 cut_to = if (!missing(cut_to)) cut_to,
                                 mix_volume = if (!missing(mix_volume)) mix_volume)))
}

#' @rdname er_layer_pause
#' @export
er_layer_detached_audio <- function(path, cut_from, cut_to, start, mix_volume) {
    do.call(er_layer, nnull(list(type = "detached-audio",
                                 path = if (!missing(path)) cpr(path),
                                 cut_from = if (!missing(cut_from)) cut_from,
                                 cut_to = if (!missing(cut_to)) cut_to,
                                 start = if (!missing(start)) start,
                                 mix_volume = if (!missing(mix_volume)) mix_volume)))
}

#' Audio track
#'
#' The difference between `er_audio_track` and [er_layer_audio()] is that an `er_audio_track` will continue to play across multiple clips and can start and stop whenever needed.
#'
#' @references <https://github.com/mifi/editly/>
#' @param path string: path to audio file
#' @param cut_from numeric: time value to cut from, in seconds
#' @param cut_to numeric: time value to cut to, in seconds
#' @param start numeric: how many seconds into the video should this audio track start? (default = 0)
#' @param mix_volume numeric: relative volume when mixing this video's audio track with others (default = 1)
#'
#' @return A list of class `er_audio_track`
#'
#' @seealso [er_header()], [er_spec()]
#'
#' @export
er_audio_track <- function(path, cut_from, cut_to, start, mix_volume) {
    out <- list()
    out <- cadd(out, path)
    out <- cadd(out, cut_from)
    out <- cadd(out, cut_to)
    out <- cadd(out, start)
    out <- cadd(out, mix_volume)
    structure(out, class = c("er_audio_track", "list"))
}

## er_layer_* functions to automatically create er_clip_* wrappers for
er_cliplist <- c("er_layer_pause", "er_layer_title", "er_layer_title_background", "er_layer_image", "er_layer_image_overlay",
"er_layer_subtitle", "er_layer_news_title", "er_layer_slide_in_text", "er_layer_fill_color", "er_layer_radial_gradient", "er_layer_linear_gradient", "er_layer_rainbow_colors", "er_layer_video", "er_layer_audio")

#' Convenience functions for clips
#'
#' Each of the `er_clip_*` functions is a wrapper around the corresponding `er_layer_*` function, creating an `er_clip` object with the structure `er_clip(duration, transition, layer)`, where:
#' * `transition` is optional and only added if it appears in the `...` parameters
#' * `layer` is created via the `er_layer_*` function, passing the appropriate parameters from `...`
#'
#' `er_clip_title2` is the same as `er_clip_title` but automatically inserts an `er_layer_fill_color` layer underneath the title, to fix font rendering issues (see <https://github.com/mifi/editly/issues/84>)
#'
#' @references <https://github.com/mifi/editly/>
#' @param duration numeric: duration in seconds
#' @param ... : as for the corresponding `er_layer_*` function
#'
#' @return A list of class `er_clip`
#'
#' @seealso [er_clip()]
#' @eval paste0("@usage ", paste0(sub("_layer_", "_clip_", er_cliplist), "(duration, ...)\n", collapse = "\n"))
#'
# @export
#' @evalRd paste("\\keyword{internal}", paste0('\\alias{', gsub('_layer_', '_clip_', er_cliplist), '}'), collapse = '\n')
#' @name er_clip_pause
#' @rdname er_clip_pause
#' @exportPattern ^er_clip_
er_clip_constructor <- function(layer_fname, layer_vars) {
    eval(str2expression(paste0("function(duration, ...) {
        rgs <- list(...)
        lyrs <- do.call(", layer_fname, ", rgs[names(rgs) %in% c(\"", paste(layer_vars, collapse = "\", \""), "\")])
        er_clip(duration = duration, transition = rgs[[\"transition\"]], layers = lyrs) }")))
}

for (layerf in er_cliplist) {
    lf_args <- formalArgs(get(layerf, mode = "function"))
    assign(sub("_layer_", "_clip_", layerf), er_clip_constructor(layerf, lf_args))
}
rm(er_clip_constructor)


#' @rdname er_clip_pause
#' @export
er_clip_title2 <- function(duration, ...) {
    rgs <- list(...)
    lyrs <- list(do.call(er_layer_fill_color, rgs[names(rgs) %in% "color"]),
                 do.call(er_layer_title, rgs[names(rgs) %in% c("text", "font_path", "text_color", "position", "zoom_direction", "zoom_amount")]))
    er_clip(duration = duration, transition = rgs[["transition"]], layers = lyrs)
}

#' Clip transitions
#'
#' A clip transitions is the *outgoing* transition from the clip.
#'
#' @references <https://github.com/mifi/editly/>
#' @param name string: can be any of the transitions listed in <https://gl-transitions.com/gallery>, or "directional-left", "directional-right", "directional-up", "directional-down", "random", " dummy"
#' @param duration numeric: duration in seconds
#' @param audio_out_curve string: fade out curve in audio cross fades (default = "tri")
#' @param audio_in_curve string: fade in curve in audio cross fades (default = "tri")
#'
#' @return A list with class `er_transition`
#'
#' @seealso [er_clip()]
#'
#' @export
er_transition <- function(name, duration, audio_out_curve, audio_in_curve) {
    out <- list(name = name)
    out <- cadd(out, duration)
    out <- cadd(out, audio_out_curve)
    out <- cadd(out, audio_in_curve)
    structure(out, class = c("er_transition", "list"))
}


# Create the JSON string ready for compiling to video by editly
#
# @references <https://github.com/mifi/editly/>
# @param header er_header: as returned by [er_header()], or a list containing the header, clips, and (optionally) audio tracks, equivalent to `list(header, clips = list(clips), audioTracks = list(audio_tracks))`
# @param clips list: a list of `er_clip` objects as returned by [er_clip()]
# @param audio_tracks list: a list of `er_audio_track` objects, as returned by [er_audio_track()]
#
# @return A json string
#
# @seealso [er_layer()], [er_exec_wait()]
#
# @export
er_spec_old <- function(header, clips, audio_tracks) {
    if (is.list(header) && "clips" %in% names(header)) {
        json <- header
    } else {
        json <- c(header, clips = list(clips))
        if (!missing(audio_tracks) && !is.null(audio_tracks)) json$audioTracks <- list(audio_tracks)
    }
    toJSON(json, auto_unbox = TRUE)
}

#' Create a spec ready for compiling to video
#'
#' @references <https://github.com/mifi/editly/>
#' @param header er_header: as returned by [er_header()]. Alternatively, do not specify the `header` argument but pass named header parameters separately
#' @param clips list: a list of `er_clip` objects as returned by [er_clip()]
#' @param audio_tracks list: a list of `er_audio_track` objects, as returned by [er_audio_track()]
#' @param theme string: name of a theme function to apply, or `NULL` for no theme. The theme will set defaults, but will not override any defaults specified in the header
#' @param as string: either "object" (return an object of class `er_spec`, suitable to pass to [er_to_json()], or "json" (the JSON string as obtained from [er_to_json()])
#' @param ... : as for `er_header`
#'
#' @return A list of class `er_spec` or a JSON string
#'
#' @seealso [er_layer()], [er_to_json()], [er_exec_wait()]
#'
#' @export
er_spec <- function(header, clips, audio_tracks, theme = "er_theme_default", as = "object", ...) {
    assert_that(is.string(as))
    as <- tolower(as)
    as <- match.arg(as, c("object", "json"))
    spec <- list()
    rgs <- list(...)
    if (missing(header) || length(header) < 1) {
        ## from dots
        h_args <- rgs[names(rgs) %in% formalArgs(er_header)]
        if (!"out_path" %in% names(h_args)) h_args$out_path <- tempfile(fileext = ".mp4")
        header <- do.call(er_header, h_args)
    }
    assert_that(inherits(header, "er_header"))

    ## apply theme to defaults
    if (!is.null(theme)) {
        if (is.string(theme)) theme <- match.fun(theme)
        assert_that(is.function(theme))
        header$defaults <- nested_merge(header$defaults, theme())
    }

    out <- c(header, clips = list(clips))
    if (!missing(audio_tracks) && !is.null(audio_tracks)) out$audioTracks <- list(audio_tracks)
    out <- structure(out, class = c("er_spec", "list"))
    if (as == "json") er_to_json(out) else out
}

#' Convert editly spec to JSON string
#'
#' @param spec string: a JSON string as returned by [er_spec()]
#'
#' @return A JSON string
#'
#' @export

er_to_json <- function(spec) toJSON(spec, auto_unbox = TRUE)


#' Editry themes
#'
#' @return An object of class `er_defaults`
#'
#' @seealso [er_defaults()], [er_spec()]
#'
#' @export
er_theme_default <- function() {
    theme_slide_colour <- "black"
    theme_text_colour <- "white"
    theme_gradient_colours <- c("#33333380", "#77777780")
    theme_gradient <- editry::er_layer_radial_gradient(colors = theme_gradient_colours)
    theme_bg_colour <- "#D02A42" ## background for news-title
    theme_resize_mode <- "contain"

    er_defaults(
        transition = list(name = "fadecolor", duration = 0.5),
        layer = list(fontPath = system.file("extdata/fonts/PatuaOne-Regular.ttf", package = "editry"),
                     textColor = theme_text_colour,
                     color = theme_slide_colour,
                     colors = theme_gradient_colours,
                     background = theme_gradient,
                     backgroundColor = theme_bg_colour,
                     resizeMode = theme_resize_mode)
    )
}
