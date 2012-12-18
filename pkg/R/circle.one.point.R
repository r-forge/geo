circle.one.point <-
function(lat, lon = NULL, rad, n = 10.)
{
	if(is.null(lon)) {
		lon <- lat$lon
		lat <- lat$lat
	}
	out <- list(lat = numeric(n), lon = numeric(n))
	dlat <- rad/60.
	dlon <- rad/arcdist(lat, lon, lat, lon - 1.)
	angles <- seq( - pi, pi, length = n)
	out$lat <- lat + dlat * sin(angles)
	out$lon <- lon + dlon * cos(angles)
	data.frame(out)
}

