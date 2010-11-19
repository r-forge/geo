geotows <-
function(lat, lon, lat1, lon1, col=1,col.names = c("kastad.n.breidd", 
	"kastad.v.lengd", "hift.n.breidd", "hift.v.lengd"), ...)
{
	if(is.data.frame(lat) && missing(lat1)) {
		lat1 <- lat[, col.names[3]]
		lon1 <- lat[, col.names[4]]
		lon <- lat[, col.names[2]]
		lat <- lat[, col.names[1]]
	}
	if(is.data.frame(lat) && !missing(lat1)) {
		lon <- lat$lon
		lat <- lat$lat
		lon1 <- lat1$lon
		lat1 <- lat1$lat
	}
	lat <- matrix(lat, length(lat), 3)
	lat[, 2] <- lat1
	lat[, 3] <- NA
	lat <- c(t(lat))
	lon <- matrix(lon, length(lon), 3)
	lon[, 2] <- lon1
	lon[, 3] <- NA
	lon <- c(t(lon))
	geolines(lat, lon,col=col, ...)
	return(invisible())
}

