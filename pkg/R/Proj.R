Proj <-
function(a, b = 0, scale = getOption("geopar")$scale,
         b0 = getOption("geopar")$b0, b1 = getOption("geopar")$b1,
         l1 = getOption("geopar")$l1,
         projection = getOption("geopar")$projection,
         col.names = c("lon", "lat"))
{
	if(col.names[1] != "lon" || col.names[2] != "lat")
		projection <- "none"
	if(is.list(a)) {
		if(projection == "none") {
			b <- a$y
			a <- a$x
		}
		else {
			b <- a$lon
			a <- a$lat
		}
	}
	if(projection == "Lambert") {
		x <- lambert(a, b, b0, l1, b1, scale, old = T)
	}
	else if(projection == "Mercator") {
		x <- mercator(a, b, scale, b0)
	}
	else if(projection == "none") {
		x <- list(x = a, y = b)
	}
}

