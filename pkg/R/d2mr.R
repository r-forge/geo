d2mr <-
function(lat, lon = NULL, dlat = 5, dlon = 10)
{
  if(is.null(lon)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon - 1e-06
  lat <- geoconvert(lat, inverse = TRUE)
  lon <-  - geoconvert(lon, inverse = TRUE)
  mlat <- lat %% 10000 %/% 100
  mlon <- lon %% 10000 %/% 100
  mlat <- mlat %/% dlat
  mlon <- mlon %/% dlon
  lat <- lat %/% 10000
  lon <- lon %/% 10000
  lat * 1000000 + mlat * 10000 + lon * 100 + mlon
}

