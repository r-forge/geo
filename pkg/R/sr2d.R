sr2d <-
function(sr)
{
  r <- floor(sr/10)
  sr <- sr - r * 10
  lat <- floor(r/100)
  lon <- (r - lat * 100) %% 50
  halfb <- (r - 100 * lat - lon)/100
  lon <-  - (lon + 0.5)
  lat <- lat + 60 + halfb + 0.25
  l1.lat <- c(0, 0.125, 0.125, -0.125, -0.125)
  l1.lon <- c(0, -0.25, 0.25, -0.25, 0.25)
  lat <- lat + l1.lat[sr + 1]
  lon <- lon + l1.lon[sr + 1]
  data.frame(lat = lat, lon = lon)
}

