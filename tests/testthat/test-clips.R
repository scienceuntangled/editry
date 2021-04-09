context("clip functions")
test_that("clip factory has worked", {
    ## we have created a bunch of er_clip_* functions automatically, check the list
    er_layer_funcs <- getNamespaceExports("editry")
    er_layer_funcs <- er_layer_funcs[grep("^er_layer_", er_layer_funcs)]
    er_layer_funcs <- setdiff(er_layer_funcs, "er_layer_detached_audio") ## no er_clip_* for these
    for (lf in er_layer_funcs) expect_type(match.fun(sub("_layer_", "_clip_", lf)), "closure")
})

