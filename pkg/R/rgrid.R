rgrid <-
function(r, fill = FALSE, ...)
{
	n <- length(r)
	lat <- r2d(r)$lat
	lon <- r2d(r)$lon
	lat <- c(rep(lat, 5), rep(NA, n))
	lon <- c(rep(lon, 5), rep(NA, n))
	lat <- as.vector(matrix(matrix(lat, nrow = 6, byrow = T), ncol = 1))
	lon <- as.vector(matrix(matrix(lon, nrow = 6, byrow = T), ncol = 1))
	lat <- lat - c(-1/4, 1/4, 1/4, -1/4, -1/4, NA)
	lon <- lon - c(-0.5, -0.5, 0.5, 0.5, -0.5, NA)
	lat <- data.frame(lat = lat, lon = lon)
	if(fill)
		geopolygon(lat, ...)
	else geolines(lat, ...)
}

