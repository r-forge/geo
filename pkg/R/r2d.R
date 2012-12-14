r2d <-
function(r)
{
	lat <- floor(r/100)
	lon <- (r - lat * 100) %% 50
	halfb <- (r - 100 * lat - lon)/100
	lon <-  - (lon + 0.5)
	lat <- lat + 60 + halfb + 0.25
	data.frame(lat = lat, lon = lon)
}

