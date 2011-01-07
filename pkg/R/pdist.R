pdist <-
function(lat, lon, lat1, lon1)
{
	rad <- 6367
	#radius of earth in km
	return(rad * acos(sin(lat) * sin(lat1) + cos(lat) * cos(lat1) * cos(
		lon - lon1)))
}

