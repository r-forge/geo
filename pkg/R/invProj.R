invProj <-
function(x, y = NULL, scale = getOption("geopar")$scale,
         b0 = getOption("geopar")$b0, b1 = getOption("geopar")$b1,
         l1 = getOption("geopar")$l1,
         projection = getOption("geopar")$projection)
{
	if(is.null(y)) {
		y <- x$y
		x <- x$x
	}
	if(projection == "Lambert") {
		x <- invlambert(x, y, b0, l1, b1, scale, old = T)
	}
	else if(projection == "Mercator") {
		x <- invmerc(x, y, scale, b0)
	}
	else if(projection == "none") {
		x <- list(x = x, y = y)
	}
}

