geoconvert.2 <-
function(lat)
{
	i <- sign(lat)
	lat <- abs(lat)
	p1 <- floor(lat)
	p2 <- floor((lat - p1) * 60)
	p3 <- round((lat - p1 - p2/60) * 100 * 60)
	return(i * (p1 * 10000 + p2 * 100 + p3))
}

