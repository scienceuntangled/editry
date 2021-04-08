context("layer functions")
test_that("layer stuff works", {
    expect_identical(er_layer("pause"), structure(list(type = "pause"), class = c("er_layer", "list")))
    expect_identical(er_layer("pause"), er_layer_pause())
    expect_identical(er_layer("pause", color = "red"), structure(list(type = "pause", color = "red"), class = c("er_layer", "list")))

    ## parms should be auto-camelCased
    expect_identical(er_layer("image", resize_mode = "aaa"), structure(list(type = "image", resizeMode = "aaa"), class = c("er_layer", "list")))
})
