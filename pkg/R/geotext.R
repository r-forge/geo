geotext <-
function(lat, lon = 0, z, cex = 0.7, adj = 0.5, col = 1, digits = 0, pretext
	 = "", lwd = 0, aftertext = "", outside = F, angle = 0, jitter = NULL,csi=NULL)
{
    if(!is.null(csi)) cex <- cex*csi/0.12 # For compatibility
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
		if(mean(lat, na.rm = T) > 1000) {
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
        if(outside)
		par(xpd = T)
	else par(xpd = F)
	if(lwd != 0)
		par(lwd = lwd)
	on.exit(par(oldpar))
	par(cex = cex)
	par(adj = adj)
	xx <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection)
	ind <- c(1:length(xx$x))
	if(is.character(z)) {
		if(pretext == "")
			txt <- z
		else txt <- paste(pretext, z, sep = "")
		if(aftertext != "")
			txt <- paste(txt, aftertext, sep = "")
	}
	else {
		if(pretext == "")
			txt <- format(round(z, digits = digits))
		else txt <- paste(pretext, format(round(z, digits = digits)),
				sep = "")
		if(aftertext != "")
			txt <- paste(txt, aftertext, sep = "")
	}
	if(!outside) {
		ind <- c(1:length(xx$x))
		ind <- ind[(!is.na(xx$x)) & (xx$x < geopar$limx[1] | xx$x >
			geopar$limx[2] | xx$y < geopar$limy[1] | xx$y > geopar$
			limy[2])]
		xx$x[ind] <- NA
		xx$y[ind] <- NA
	}
	if(length(angle) == length(xx$x) || length(col) == length(xx$x)) {
		if(length(angle) < length(xx$x))
			angle <- rep(angle[1], length(xx$x))
		if(length(col) < length(xx$x))
			col <- rep(col[1], length(xx$x))
		for(i in 1:length(xx$x)) {
			text(xx$x[i], xx$y[i], txt, col = col[i], srt = angle[
				i])
		}
	}
	else text(xx$x, xx$y, txt, col = col, srt = angle)
	return(invisible())
}

