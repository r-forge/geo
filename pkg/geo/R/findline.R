findline <-
function(x, xb, plot = T)
{
	if(!plot) {
		x <- Proj(x)
		xb <- Proj(xb)
	}
	xr <- yr <- rep(0, 3 * (length(x$y) + length(xb$y)))
	nxr <- 0
	ab <- ab1 <- rep(0, length(xb$x))
	li <- prepare.line(x$x)
	ind <- c(1:length(x$x))
	ind <- ind[is.na(x$x)]
	if(length(ind) > 0)
		x$x[ind] <- x$y[ind] <- -999999
	xr <- .C("define_multiline",
		as.double(x$x),
		as.double(x$y),
		as.double(xb$x),
		as.double(xb$y),
		as.double(xr),
		as.double(yr),
		as.integer(length(x$y)),
		as.integer(length(xb$y)),
		as.integer(nxr),
		as.integer(li$lx1),
		as.integer(li$lx2),
		as.integer(li$nlx),
		as.integer(plot),
		as.double(ab),
		as.double(ab1))
	nxr <- xr[[9]]
	yr <- xr[[6]]
	yr <- yr[1:nxr]
	xr <- xr[[5]]
	xr <- xr[1:nxr]
	ind <- c(1:nxr)
	ind <- ind[xr < -999998]
	xr[ind] <- NA
	yr[ind] <- NA
	if(!plot) {
		xr <- invProj(xr, yr)
		xr <- data.frame(list(lat = xr$lat, lon = xr$lon))
		return(invisible(xr))
	}
	else return(list(y = yr, x = xr, nxr = nxr))
}

