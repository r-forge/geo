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
	r <- (floor(lat) - 60) * 100 + floor(lon)
	ifelse(lat - floor(lat) > 0.5, r + 50, r)
}

