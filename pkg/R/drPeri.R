drPeri <-
function(dr, dlat = 1, dlon = 2)
{
  lat <- dr2d(dr, dlat = dlat, dlon = dlon)$lat
  lon <- dr2d(dr, dlat = dlat, dlon = dlon)$lon
  lat <- lat + c(dlat/2, dlat/2,  - dlat/2,  - dlat/2, dlat/2)
  lon <- lon + c(dlon/2,  - dlon/2,  - dlon/2, dlon/2, dlon/2)
  data.frame(lat = lat, lon = lon)
}

