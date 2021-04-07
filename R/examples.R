if (FALSE) {
    clips<- list(
        er_clip(duration = 3, layers = er_layer("title", text = "Title slide")),
        er_clip(duration = 3, layers = er_layer("image", path = system.file("extdata/images/Rlogo.png", package = "editry"), resizeMode = "contain", zoomDirection = "out"))
    )
    outf <- tempfile(fileext = ".mp4")
    hdr <- er_header(out_path = outf)
    json1 <- er_spec(header = hdr, clips = clips)
    er_exec_wait(json = json1)
}

## {
##     fps: 30,
##     outPath: './test.mp4',
##     defaults: {
## 	transition: { name: 'fadecolor', duration: 0.25 },
## 	layer: { fontPath: './PatuaOne-Regular.ttf' },
## 	layerType: { 'pause': { color: '#000000' } },
##     },
##     clips: [
## 	{ duration: 3, layers: [{ type: 'image', path: './vtblur.png', resizeMode: 'contain', zoomDirection: 'out' },
## 				{ type: 'title-background', text: 'Under 17 Women\n2020 Season Highlights', background: { type: 'linear-gradient', colors: ['#777777A0', '#555555A0'] } }, //#22794f
## 				//{ type: 'image-overlay', path: './vtas-recreated.png', position: { x: 0.03, y: 0.03}, start: 0.4, width: 0.25 }, //stop: 1.5,
## 			       ] },
## 	{ duration: 2, layers: [{ type: 'pause' }, { type: 'title', text: 'Attacking' }] },
## 	{ duration: 1, layers: [{ type: 'pause' }] },
##     ],
## }
