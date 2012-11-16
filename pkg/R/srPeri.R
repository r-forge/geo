srPeri <-
function(sr)
{
  lat <- sr2d(sr)$lat
  lon <- sr2d(sr)$lon
  lat <- lat + c(1/8, 1/8,  - 1/8,  - 1/8, 1/8)
  lon <- lon + c(0.25,  - 0.25,  - 0.25, 0.25, 0.25)
  data.frame(lat = lat, lon = lon)
}

