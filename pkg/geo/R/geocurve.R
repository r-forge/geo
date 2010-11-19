geocurve <-
function(data, df = nrow(data)/2, n = 10, open = T, arrow = "none", col = 1,
	lwd = 1, size = 0.2, angle = 15, smooth = T, plot = T, ...)
{
	if(df == "all")
		df <- nrow(data) - 1
	if(smooth) {
		if(open)
			data <- Open.curve(data, df, n)
		else {
			n <- nrow(data)
			if(data[1, "lat"] != data[n, "lat"] || data[1, "lon"] !=
				data[n, "lon"])
				data <- rbind(data, data[1,  ])
			data <- Closed.curve(data, df, n)
		}
	}
	if(plot) {
		geolines(data, lwd = lwd, col = col, ...)
		if(arrow == "start" || arrow == "both")
			geolines.with.arrows(data, start = T, col = col, size
				 = size, angle = angle)
		if(arrow == "end" || arrow == "both")
			geolines.with.arrows(data, start = F, col = col, size
				 = size, angle = angle)
	}
	return(invisible(data))
}

