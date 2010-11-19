geolocator <-
function(type = "p", n = 0)
{
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	if(n == 0)
		x <- locator(type = type)
	else x <- locator(type = type, n = n)
	if(!is.null(x$x)) {
		lat <- invProj(x$x, x$y, geopar$scale, geopar$b0, geopar$b1,
			geopar$l1, projection = geopar$projection)
		if(geopar$projection == "none")
			return(x <- data.frame(x = lat$x, y = lat$y))
		else return(lat <- data.frame(lat = lat$lat, lon = lat$lon))
	}
	else return(list())
}

