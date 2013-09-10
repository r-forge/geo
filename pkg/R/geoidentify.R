geoidentify <-
function(lat, lon = NULL, labels = 1, n = 0, plot = TRUE, atpen = TRUE, offset = 0.5,
	col = 1, cex = 1)
{
	geopar <- getOption("geopar")
	oldpar <- selectedpar()
	par(geopar$gpar)
	par(cex = cex)
	par(col = col)
	on.exit(par(oldpar))
	if(is.null(lon)) {
		if(geopar$projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	if(geopar$projection != "none") {
		# degrees and minutes
		if(mean(lat, na.rm = TRUE) > 1000) {
			lat <- geoconvert(lat)
			lon <-  - geoconvert(lon)
		}
	}
	if(length(labels) == 1 && length(lat) > 1)
		labels <- seq(along = lat)
	if(n == 0)
		n <- length(lat)
	xx <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection)
	z <- identify(xx$x, xx$y, labels = labels, n = n, plot = plot, atpen = 
		atpen, offset = offset)
	return(z)
}

