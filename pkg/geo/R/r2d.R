r2d <-
function(square)
{
	lat <- floor(square/100)
	lon <- (square - lat * 100) %% 50
	halfb <- (square - 100 * lat - lon)/100
	lon <-  - (lon + 0.5)
	lat <- lat + 60 + halfb + 0.25
	return(list(lat = lat, lon = lon))
}

