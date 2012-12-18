intra.point.dist <-
function(x)
{
	n <- length(x$lat)
	arcdist(x$lat[ - n], x$lon[ - n], x$lat[-1], x$lon[-1])
}

