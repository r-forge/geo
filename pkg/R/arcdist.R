arcdist <-
function(lat, lon, lat1 = NULL, lon1 = NULL, scale = "nmi")
{
	if(is.null(lat1)) {
		lat1 <- lon$lat
		lon1 <- lon$lon
		lon <- lat$lon
		lat <- lat$lat
	}
	if(scale == "nmi")
		miles <- 1.852
	else miles <- 1
	rad <- 6367
	#radius of earth in km
	mult1 <- (rad/miles)
	mult2 <- pi/180
	return(mult1 * acos(sin(mult2 * lat) * sin(mult2 * lat1) + cos(mult2 *
		lat) * cos(mult2 * lat1) * cos(mult2 * lon - mult2 * lon1)))
}

