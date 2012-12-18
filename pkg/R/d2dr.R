d2dr <-
function(lat, lon = NULL, dlat = 1, dlon = 2)
{
  if(is.null(lon)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon - 1e-06
  hemi <- sign(lon)
  lat <- floor(lat)%%60
  lon <- floor(lon)
  hemi*(100*lat%/%dlat + hemi*floor(lon/dlon))
}

