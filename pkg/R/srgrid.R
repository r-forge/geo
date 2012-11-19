srgrid <-
function(sr, fill = F, ...)
{
	n <- length(sr)
	lat <- sr2d(sr)$lat
	lon <- sr2d(sr)$lon
	lat <- c(rep(lat, 5), rep(NA, n))
	lon <- c(rep(lon, 5), rep(NA, n))
	lat <- as.vector(matrix(matrix(lat, nrow = 6, byrow = T), ncol = 1))
	lon <- as.vector(matrix(matrix(lon, nrow = 6, byrow = T), ncol = 1))
	lat <- lat - c(-1/8, 1/8, 1/8, -1/8, -1/8, NA)
	lon <- lon - c(-0.25, -0.25, 0.25, 0.25, -0.25, NA)
	lat <- data.frame(lat = lat, lon = lon)
	if(fill)
		geopolygon(lat, ...)
	else geolines(lat, ...)
}

