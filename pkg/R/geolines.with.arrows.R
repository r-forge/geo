geolines.with.arrows <-
function(data, start = T, size = 0.2, ...)
{
	geopar <- getOption("geopar")
	if(!is.data.frame(data))
		data <- data.frame(data)
	n <- nrow(data)
	if(start)
		i <- c(1:n)
	else i <- seq(n, 1, by = -1)
	tmpdata <- data[i,  ]
	limits <- invProj(geopar$limx, geopar$limy)
	plt.size <- geopar$gpar$pin
	dlon <- size/plt.size[1] * diff(limits$lon)
	dlat <- size/plt.size[2] * diff(limits$lat)
	theta <- seq(0, 2 * pi, by = 0.1)
	lat <- tmpdata[1, "lat"] + dlat * sin(theta)
	lon <- tmpdata[1, "lon"] + dlon * cos(theta)
	circle <- data.frame(lat = lat, lon = lon)
	xr <- findline(tmpdata, circle, plot = F)
	i <- is.na(xr$lat)
	i1 <- c(1:length(i))
	i1 <- i1[i]
	n <- min(i1) - 1
	pos <- list(lat = c(xr$lat[n], tmpdata$lat[1]), lon = c(xr$lon[n],
		tmpdata$lon[1]))
	pos <- Arrow(pos, ...)
	return(invisible(pos))
}

