geolines <-
function(lat, lon = 0, col = 1, lwd = 0, lty = 0, nx = 1, outside = FALSE, 
	return.data = FALSE)
{
	if(length(lon) == 1) {
		# For polygon structures.
		if(!is.null(lat$length)) n <- lat$length else n <- max(c(length(
				lat$y), length(lat$lat)))
		if(geopar$projection == "none") {
			lon <- lat$y[1:n]
			lat <- lat$x[1:n]
		}
		else {
			lon <- lat$lon[1:n]
			lat <- lat$lat[1:n]
		}
	}
	if(geopar$projection != "none") {
		# degrees and minutes
		if(mean(lat, na.rm = TRUE) > 1000) {
			lat <- geoconvert(lat)
			lon <-  - geoconvert(lon)
		}
	}
	if(outside)
		par(xpd = TRUE)
	else par(xpd = FALSE)
	if(nx > 1) {
		# fill in with points for lambert. 
		x <- fill.points(lat, lon, nx, option = 2)
		lat <- x$x
		lon <- x$y
	}
	oldpar <- selectedpar()
	par(geopar$gpar)
	if(lwd != 0)
		par(lwd = lwd)
	if(lty != 0)
		par(lty = lty)
	on.exit(par(oldpar))
	xx <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection)
	if(!outside) {
		gx <- geopar$limx
		gy <- geopar$limy
		border <- list(x = c(gx[1], gx[2], gx[2], gx[1], gx[1]), y = c(
			gy[1], gy[1], gy[2], gy[2], gy[1]))
		xx <- findline(xx, border)
	}
	else par(xpd = FALSE)
	#c program not used
	lines(xx$x, xx$y, col = col)
	par(oldpar)
	if(return.data) {
		xx <- invProj(xx)
		xx <- data.frame(lat = xx$lat, lon = xx$lon)
		return(invisible(xx))
	}
	else return(invisible())
}

