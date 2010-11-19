setgrid <-
function(lat, lon = 0, type = "p", pch = "*", xlim = c(0, 0), ylim = c(0, 0),
	b0 = 65, r = 1.1, country = island, xlab = "default", ylab = "default",
	option = "cut", reg = 0, dx = c(0, 0), nx = c(0, 0), grpkt = 0, scale
	 = "km", find = F, new = F, grid = T, projection = "Mercator", n = 2500,
	b1 = b0, nholes = 0)
{
	if(length(lon) == 1) {
		if(projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	geoplot(lat, lon, type = type, pch = pch, xlim = xlim, ylim = ylim,
		b0 = b0, r = r, country = country, xlab = xlab, ylab = ylab,
		option = option, new = new, grid = grid, projection = 
		projection, b1 = b1)
	# Find borders either given or with the locator.  
	oldpar <- selectedpar()
	par(geopar$gpar)
	# set graphical parameters
	on.exit(par(oldpar))
	if(length(reg) == 1) {
		# use the locator.  
		reg <- define.area(nholes = nholes)
	}
	xgr <- gridpoints(reg, dx, grpkt, nx, n)
	# grid points.  
	grpt <- xgr$xgr
	xgr <- xgr$xgra
	# change names
	geopoints(xgr, pch = ".")
	# 	Find what is inside the borders.  
	if(find) {
		xgr <- inside(xgr, reg = reg)
		geopoints(xgr, pch = "+")
		return(list(xgr = xgr, grpt = grpt, reg = reg, find = find))
	}
	else return(list(grpt = grpt, reg = reg, find = find))
}

