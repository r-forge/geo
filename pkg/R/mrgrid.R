mrgrid <-
function(mr, dlat = 5, dlon = 10, fill = FALSE, ...)
{
  n <- length(mr)
  lat <- mr2d(mr, dlat = dlat, dlon = dlon)$lat
  lon <- mr2d(mr, dlat = dlat, dlon = dlon)$lon
  lat <- c(rep(lat, 5), rep(NA, n))
  lon <- c(rep(lon, 5), rep(NA, n))
  lat <- as.vector(matrix(matrix(lat, nrow = 6, byrow = T), ncol = 1))
  lon <- as.vector(matrix(matrix(lon, nrow = 6, byrow = T), ncol = 1))
  lat <- lat + c(dlat/120, dlat/120,  - dlat/120,  - dlat/120, dlat/
  	120, NA)
  lon <- lon + c(dlon/120,  - dlon/120,  - dlon/120, dlon/120, dlon/
  	120, NA)
  lat <- data.frame(lat = lat, lon = lon)
  if(fill)
  	geopolygon(lat, ...)
  else geolines(lat, ...)
}

