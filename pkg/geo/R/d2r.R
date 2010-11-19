d2r <-
function(lat, lon = NULL)
{
	if(is.null(lon)) {
		lon <- lat$lon
		lat <- lat$lat
	}
	lat <- lat + 1e-06
	lon <- lon - 1e-06
	lon <-  - lon
	reit <- (floor(lat) - 60) * 100 + floor(lon)
	reit <- ifelse(lat - floor(lat) > 0.5, reit + 50, reit)
	return(reit)
}

