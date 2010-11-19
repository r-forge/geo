fill.outside.border <-
function(col = 0, rat = 1)
{
	gx <- geopar$limx
	gy <- geopar$limy
	gx <- mean(gx) + rat * (gx - mean(gx))
	gy <- mean(gy) + rat * (gy - mean(gy))
	dx <- gx[2] - gx[1]
	dy <- gy[2] - gy[1]
	x1 <- gx[1] - dx
	x2 <- gx[2] + dx
	y1 <- gy[1] - dy
	y2 <- gy[2] + dy
	b1 <- list(x = c(x1, x2, x2, x1, x1), y = c(gy[2], gy[2], y2, y2, gy[
		2]))
	b2 <- list(x = c(x1, x2, x2, x1, x1), y = c(gy[1], gy[1], y1, y1, gy[
		1]))
	b3 <- list(x = c(gx[2], x2, x2, gx[2], gx[2]), y = c(gy[1], gy[1],
		gy[2], gy[2], gy[1]))
	b4 <- list(x = c(gx[1], x1, x1, gx[1], gx[1]), y = c(gy[1], gy[1],
		gy[2], gy[2], gy[1]))
	oldpar <- selectedpar()
	par(geopar$gpar)
	polygon(b1, col = 0)
	polygon(b2, col = 0)
	polygon(b3, col = 0)
	polygon(b4, col = 0)
	par(oldpar)
	return(invisible())
}

