selpos <-
function(lat, lon = NULL, ind)
{
	if(is.null(lon)) {
		lon <- lat$lon
		lat <- lat$lat
	}
	lat <- lat[ind]
	lon <- lon[ind]
	return(lat, lon)
}

