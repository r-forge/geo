geogrid <-
function(lat, lon = 0, col = 1, type = "l", lwd = 0, lty = 0, pch = "+")
{
	oldpar <- selectedpar()
	#par(geopar$gpar)
	if(length(lon) == 1) {
		if(geopar$projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	if(geopar$projection == "Lambert")
		nx <- 10
	else nx <- 1
	if(geopar$projection != "none") {
		# degrees and minutes
		if(mean(lat, na.rm = T) > 1000) {
			lat <- geoconvert(lat)
			lon <-  - geoconvert(lon)
		}
	}
	if(type == "l") {
		#		lat <- c(lat,NA) ; lon <- c(lon,NA)
		llon <- length(lon)
		llat <- length(lat)
		latgr <- t(matrix(lat, llat, llon))
		longr <- matrix(lon, llon, llat)
		n1 <- rep(NA, nrow(latgr))
		n2 <- rep(NA, ncol(latgr) + 1)
		latgr <- cbind(latgr, n1)
		latgr <- rbind(latgr, n2)
		longr <- cbind(longr, n1)
		longr <- rbind(longr, n2)
		geolines(latgr, longr, col = col, nx = nx)
		# plot the lines.
		llon <- length(lon)
		llat <- length(lat)
		latgr <- matrix(lat, llat, llon)
		longr <- t(matrix(lon, llon, llat))
		n1 <- rep(NA, nrow(latgr))
		n2 <- rep(NA, ncol(latgr) + 1)
		latgr <- cbind(latgr, n1)
		latgr <- rbind(latgr, n2)
		longr <- cbind(longr, n1)
		longr <- rbind(longr, n2)
		geolines(latgr, longr, col = col, lwd = lwd, lty = lty, nx = nx
			)
	}
	else {
		llon <- length(lon)
		llat <- length(lat)
		latgr <- c(t(matrix(lat, llat, llon)))
		longr <- c(matrix(lon, llon, llat))
		geopoints(latgr, longr, pch = pch)
	}
	return(invisible())
	par(oldpar)
}

