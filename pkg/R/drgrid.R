drgrid <-
function(dr, dlat = 1, dlon = 2, fill = FALSE, ...)
{
  n <- length(dr)
  lat <- dr2d(dr, dlat = dlat, dlon = dlon)$lat
  lon <- dr2d(dr, dlat = dlat, dlon = dlon)$lon
  lat <- c(rep(lat, 5), rep(NA, n))
  lon <- c(rep(lon, 5), rep(NA, n))
  lat <- as.vector(matrix(matrix(lat, nrow = 6, byrow = T), ncol = 1))
  lon <- as.vector(matrix(matrix(lon, nrow = 6, byrow = T), ncol = 1))
  lat <- lat + c(dlat/2, dlat/2,  - dlat/2,  - dlat/2, dlat/
  	2, NA)
  lon <- lon + c(dlon/2,  - dlon/2,  - dlon/2, dlon/2, dlon/
  	2, NA)
  lat <- data.frame(lat = lat, lon = lon)
  if(fill)
  	geopolygon(lat, ...)
  else geolines(lat, ...)
}

