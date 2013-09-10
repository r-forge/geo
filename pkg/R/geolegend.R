geolegend <-
function(pos, legend, ...)
{
	geopar <- getOption("geopar")
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	xx <- Proj(pos$lat, pos$lon)
	legend(xx$x, xx$y, legend = legend, ...)
}

