editry_app_dir <- function() rappdirs::user_data_dir(appname = "editry")

## also this one, since we can use the ffmpeg installed there if it exists
ovideo_app_dir <- function() rappdirs::user_data_dir(appname = "ovideo")
