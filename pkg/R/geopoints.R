geopoints <-
function(lat, lon = 0, pch = "*",cex =0.7, col = 1, lwd = 0, outside = FALSE,
	jitter = NULL, mkh = NULL,csi=NULL)
{
   if(!is.null(csi)) cex <- cex*csi/0.12  # Compatibility with old program
	if(length(lon) == 1 && length(lat) > 1) {
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
	if(!is.null(jitter)) {
		lat <- lat + runif(length(lat), -1, 1) * jitter
		lon <- lon + runif(length(lon), -1, 1) * jitter * 2
	}
	oldpar <- selectedpar()
	par(geopar$gpar)
	if(lwd != 0)
		par(lwd = lwd)
	if(outside)
		par(xpd = TRUE)
	else par(xpd = FALSE)
	on.exit(par(oldpar))
	par(cex = cex)
	xx <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection)
	if(!outside) {
		ind <- c(1:length(lat))
		ind <- ind[xx$x > geopar$limx[2] | xx$x < geopar$limx[1] | xx$
			y < geopar$limy[1] | xx$y > geopar$limy[2] | is.na(
			xx$x) | is.na(xx$y)]
		if(length(ind) > 0) {
			xx$x <- xx$x[ - ind]
			xx$y <- xx$y[ - ind]
		}
	}
	if(!is.null(mkh))
		points(xx$x, xx$y, pch = pch, col = col, mkh = mkh)
	else points(xx$x, xx$y, pch = pch, col = col)
	return(invisible())
}

