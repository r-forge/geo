find.hnit <-
function(pt, poly)
{
	pt1 <- floor(pt)
	pt2 <- pt - pt1
	y <- poly$y[pt1] + pt2 * (poly$y[pt1 + 1] - poly$y[pt1])
	x <- poly$x[pt1] + pt2 * (poly$x[pt1 + 1] - poly$x[pt1])
	return(data.frame(x = x, y = y))
}

