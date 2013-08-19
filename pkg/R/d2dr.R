d2dr <-
function(lat, lon = NULL, dlat = 1, dlon = 2, startLat = 50)
{
  if(is.null(lon)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon - 1e-06
  hemi <- sign(lon)
  lat <- floor(lat)%%startLat
  lon <- floor(lon)
  hemi*(100*lat%/%dlat + hemi*floor(lon/dlon))
}

