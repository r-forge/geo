geodefine <-
function(nholes = 0)
{
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	border <- giveborder(nholes = nholes)
	reg <- border$reg
	par(oldpar)
	if(geopar$projection == "none")
		reg <- list(x = reg$x, y = reg$y)
	else reg <- list(lat = reg$lat, lon = reg$lon)
	reg <- data.frame(reg)
	return(reg)
}

