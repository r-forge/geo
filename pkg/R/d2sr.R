d2sr <-
function(lat, lon = NULL)
{
  if(is.null(lon)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon - 1e-06
  lon <-  - lon
  r <- (floor(lat) - 60) * 100 + floor(lon)
  r <- ifelse(lat - floor(lat) > 0.5, r + 50, r)
  deg <- r2d(r)
  lon <-  - lon
  dlat <-  - (lat - deg$lat)
  dlon <-  - (lon - deg$lon)
  dl <- sign(dlat + 1e-07) + 2 * sign(dlon + 
    1e-07) + 4
  sr <- c(2, 0, 4, 0, 1, 0, 3)
  sr <- sr[dl]
  floor(r * 10 + sr)
}

