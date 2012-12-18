dist.mat <-
function(lat, lon, lat1 = NULL, lon1 = NULL, Scale = "nmi")
{
	if(is.null(lat1)) {
		lon1 <- lon$lon
		lat1 <- lon$lat
		if(is.data.frame(lon))
			jnames <- row.names(lon)
		lon <- lat$lon
		if(is.data.frame(lat))
			inames <- row.names(lat)
		lat <- lat$lat
	}
	inames <- names(lat)
	jnames <- names(lat1)
	nx <- length(lat)
	ny <- length(lat1)
	if(Scale == "nmi")
		miles <- 1.8520000000000001
	else miles <- 1
	rad <- 6367
	#radius of earth in km
	mult1 <- (rad/miles)
	mult2 <- pi/180
	lat <- matrix(lat, nx, ny) * mult2
	lon <- matrix(lon, nx, ny) * mult2
	lat1 <- matrix(lat1, nx, ny, byrow = T) * mult2
	lon1 <- matrix(lon1, nx, ny, byrow = T) * mult2
	result <- mult1 * (acos(sin(lat) * sin(lat1) + cos(lat) * cos(lat1) *
		cos(lon - lon1)))
	dimnames(result) <- list(inames, jnames)
	result
}

