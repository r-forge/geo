mrPeri <-
function(mr, dlat = 5, dlon = 10)
{
  lat <- mr2d(mr, dlat = dlat, dlon = dlon)$lat
  lon <- mr2d(mr, dlat = dlat, dlon = dlon)$lon
  lat <- lat + c(dlat/120, dlat/120,  - dlat/120,  - dlat/120, dlat/120)
  lon <- lon + c(dlon/120,  - dlon/120,  - dlon/120, dlon/120, dlon/120)
  data.frame(lat = lat, lon = lon)
}

