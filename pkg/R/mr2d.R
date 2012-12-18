mr2d <-
function(mr, dlat = 5, dlon = 10)
{
  lat <- mr %/% 1000000
  mr <- mr %% 1000000
  mlat <- mr %/% 10000
  mr <- mr %% 10000
  lon <- mr %/% 100
  mlon <- mr %% 100
  lat <- 10000 * lat + 100 * (dlat * mlat + dlat/2)
  lon <- 10000 * lon + 100 * (dlon * mlon + dlon/2)
  data.frame(lat = geoconvert(lat), lon =  - geoconvert(lon))
}

