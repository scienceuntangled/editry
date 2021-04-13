#' Default editry theme
#'
#' Themes in editry are a set of defaults that can be supplied to the [er_header()] or [er_spec()] functions.
#'
#' The default theme currently uses:
#' * white text, with 'PatuaOne' font face
#' * black slide backgrounds for plain slides, and grey gradients for radial or linear gradient backgrounds
#' * red background for news-title layers
#' * resize_mode "contain" for images
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
