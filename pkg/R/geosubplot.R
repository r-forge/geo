geosubplot <-
function(fun, pos, size = c(2, 2), fill, fillcol, ...)
{
	geopar <- getOption("geopar")
	if(length(pos$lat) == 1) {
		# Calculate new limits.
		plt.size <- par()$pin
		rlon <- (diff(geopar$origin$lon) * size[1])/plt.size[1]
		rlat <- (diff(geopar$origin$lat) * size[2])/plt.size[2]
		pos <- data.frame(lat = pos$lat + c(-0.5, 0.5) * rlat, lon = 
			pos$lon + c(-0.5, 0.5) * rlon)
	}
	if(!missing(fill)) {
		if(!missing(fillcol))
			geopolygon(pos, col = fillcol)
		else geopolygon(pos, col = 0)
	}
	pos <- Proj(pos)
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	pr <- subplot(fun, pos, ...)
	return(invisible())
}

