plot_reitnr <-
function(cexrt, lwd = 0)
{
	lat <- invProj(geopar$limx, geopar$limy, geopar$scale, geopar$b0, 
		geopar$b1, geopar$l1, geopar$projection)
	minlat <- floor(lat$lat[1] * 2)/2 - 0.5
	minlon <- floor(lat$lon[1]) - 1
	maxlon <- floor(lat$lon[2]) + 1
	maxlat <- floor(lat$lat[2] * 2)/2 + 0.5
	nlat <- (maxlat - minlat) * 2 + 1
	nlon <- (maxlon - minlon) + 1
	lon <- minlon + c(0:nlon)
	lat <- minlat + c(0:nlat) * 0.5
	lon <- lon + 0.5
	lat <- lat + 0.25
	nlat <- length(lat)
	nlon <- length(lon)
	lat <- c(matrix(lat, nlat, nlon))
	lon <- c(t(matrix(lon, nlon, nlat)))
	z <- d2r(lat, lon)
	geotext(lat, lon, z, cex = cexrt, lwd = lwd)
}

