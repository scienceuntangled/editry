context("spec functions")
test_that("spec stuff works", {
    clips <- list(
        er_clip_title2(duration = 3, text = "Title slide", color = "black", transition = er_transition(name = "windowslice")),
        er_clip_image(duration = 3, path = "https://jeroen.github.io/images/Rlogo.png",
                      resize_mode = "contain", zoom_direction = "out"))

    outfile <- tempfile(fileext = ".mp4")
    my_spec <- er_spec(clips = clips , out_path = outfile, allow_remote_requests = TRUE)
    ##er_exec_wait(spec = my_spec, fast = TRUE)
    my_spec2 <- er_spec(clips = clips , out_path = outfile, allow_remote_requests = TRUE, theme = NULL)
    expect_true("defaults" %in% names(my_spec))
    expect_false("defaults" %in% names(my_spec2))
})
