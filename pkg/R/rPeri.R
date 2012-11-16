rPeri <-
function(r)
{
  lat <- r2d(r)$lat
  lon <- r2d(r)$lon
  lat <- lat + c(1/4, 1/4,  - 1/4,  - 1/4, 1/4)
  lon <- lon + c(0.5,  - 0.5,  - 0.5, 0.5, 0.5)
  data.frame(lat = lat, lon = lon)
}

