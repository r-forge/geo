adjust.grd <-
function(ply, rat = 0.025)
{
	gx <- geopar$limx
	gy <- geopar$limy
	bx1 <- list(x = c(gx[1], gx[2], gx[2], gx[1], gx[1]), y = c(gy[1],
		gy[1], gy[2], gy[2], gy[1]))
	gx <- mean(gx) + (1 - rat) * (gx - mean(gx))
	gy <- mean(gy) + (1 - rat) * (gy - mean(gy))
	bx1 <- list(x = c(gx[1], gx[2], gx[2], gx[1], gx[1], bx1$x), y = c(
		gy[1], gy[1], gy[2], gy[2], gy[1], bx1$y))
	ply <- findline(ply, bx1)
	return(ply)
}

