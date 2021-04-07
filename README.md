
<!-- README.md is generated from README.Rmd. Please edit that file -->

# editry

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`editry` provides R bindings to
[editly](https://github.com/mifi/editly/), a “tool and framework for
declarative NLE (non-linear video editing) using Node.js and ffmpeg.
Editly allows you to easily and programmatically create a video from a
set of clips, images, audio and titles, with smooth transitions and
music overlaid.”

## Installation

You can install from
[GitHub](https://github.com/scienceuntangled/editry) with:

``` r
## install.packages("remotes") ## if needed
remotes::install_github("scienceuntangled/editry")
```

Before `editry` can be used, you also need to install `node`, and the
`editly` npm package. You only need to do these steps once.

Install `node`:

``` r
noder::nr_install_node()
```

Then `editly`:

``` r
library(editry)
er_install_editly()
```

If you don’t have `ffmpeg` on your system, you will also need this. On
Windows you can use:

``` r
er_install_ffmpeg()
```

On other platforms (Linux, Mac) you will need to install `ffmpeg`
yourself, e.g. using your package manager.

## Example usage

The sequence of operations that editly performs to produce a video is
called an “edit spec”. See the [editly GitHub
page](https://github.com/mifi/editly/) for details and examples.

Typically a spec consists of a sequence of clips, along with some header
parameters that control aspects of the overall video. Each clip contains
one or more layers. Let’s start with two simple clips:

``` r
clips <- list(
    er_clip(duration = 3, layers = er_layer("title", text = "Title slide")),
    er_clip(duration = 3, layers = er_layer("image",
                                            path = system.file("extdata/images/Rlogo.png", package = "editry"),
                                            resizeMode = "contain", zoomDirection = "out"))
)
```

The header:

``` r
outfile <- tempfile(fileext = ".mp4")
hdr <- er_header(out_path = outfile)
```

Now we can build the actual spec (which is a json string):

``` r
my_json <- er_spec(header = hdr, clips = clips)
```

And compile this into a video. We use the `fast` option here to generate
a low-quality preview:

``` r
er_exec_wait(json = my_json, fast = TRUE)
#> ffmpeg version 4.2.4
#> ffprobe version 4.2.4
#> 250x250 15fps
#>   0%  12%  24%  36%  48% Done with transition, switching to next transitionFromClip (1)
#>  60%  72%  84%  96% Done with transition, switching to next transitionFromClip (2)
#> No more transitionFromClip, done
#> 
#> Done. Output file can be found at:
#> /tmp/RtmpYkVLzn/filea3693e03335e.mp4
#> [1] 0
```

<video src="man/figures/example1.mp4" controls autoplay loop />
