geopolygon <-
function(lat, lon = NULL, col = "white", border = F, exterior = F, nx = 1,
	outside = F, plot = T, save = F, rat = 0.005, density = -1, Projection
	 = NULL, angle = 45, allowed.size = 80000, option = 1)
{
	if(is.null(Projection))
		Projection <- geopar$projection
	# 	for structures too large for hardware
	index <- lat$index
	RANGE <- lat$range
	LENGTH <- lat$length
	if(exterior)
		in.or.out <- 1
	else in.or.out <- 0
	err <- F
	if(is.null(lon)) {
		if(Projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	if(Projection != "none") {
		# degrees and minutes
		if(mean(lat, na.rm = T) > 1000) {
			lat <- geoconvert(lat)
			lon <-  - geoconvert(lon)
		}
	}
	if(length(lat) == 2) {
		lat <- c(lat[1], lat[1], lat[2], lat[2], lat[1])
		lon <- c(lon[1], lon[2], lon[2], lon[1], lon[1])
	}
	if(nx > 1) {
		# fill in with points for lambert. 
		x <- fill.points(lat, lon, nx, option = 2)
		lat <- x$x
		lon <- x$y
	}
	oldpar <- selectedpar()
	par(geopar$gpar)
	if(outside)
	par(xpd = T)
	else par(xpd = F)
	on.exit(par(oldpar))
	gx <- geopar$limx
	rx <- gx[2] - gx[1]
	gy <- geopar$limy
	ry <- gy[2] - gy[1]
	gx[1] <- gx[1] + rat * rx
	gx[2] <- gx[2] - rat * ry
	gy[1] <- gy[1] + rat * ry
	gy[2] <- gy[2] - rat * ry
	brd <- data.frame(x = c(gx[1], gx[2], gx[2], gx[1], gx[1]), y = c(
		gy[1], gy[1], gy[2], gy[2], gy[1]))
	brd1 <- invProj(brd)
	brd1 <- data.frame(lat = brd1$lat, lon = brd1$lon)
	if(!is.null(index)) {
		limits <- invProj(geopar$limx, geopar$limy)
		for(i in 1:length(index)) {
			xx <- Proj(lat[index[[i]]], lon[index[[i]]])
			if(!outside)
				xx <- cut.multipoly(xx, brd, in.or.out)
			if(length(xx$x) > 0)
				polygon(xx$x, xx$y, col = col, border = border,
					density = density, angle = angle)
		}
		return(invisible())
	}
	else {
		if(exterior) {
			i1 <- geoinside(brd1, data.frame(lat = lat, lon = lon),
				option = 0)
			i <- 1:4
			i <- i[is.na(match(i, i1))]
			if(length(i) == 0)
				return(invisible())
			else i1 <- i[1]
			i <- geoinside(data.frame(lat = lat, lon = lon), brd1,
				na.rm = T, robust = F, option = 0)
			if(length(i) == length(lat) || option != 1) {
				lat <- lat[!is.na(lat)]
				lon <- lon[!is.na(lon)]
				dist <- (lat - brd1$lat[i1])^2 + (lon - brd1$
					lon[i1])^2 * cos((mean(lat) * pi)/
					180)^2
				o <- order(dist)
				lat <- c(lat[c(o[1]:length(lat), 1:o[1])],
					brd1$lat[c(i1:4, 1:i1)], lat[o[1]])
				lon <- c(lon[c(o[1]:length(lon), 1:o[1])],
					brd1$lon[c(i1:4, 1:i1)], lon[o[1]])
				xx <- Proj(lat, lon, geopar$scale, geopar$
					b0, geopar$b1, geopar$l1, Projection)
				if(plot) {
					polygon(xx$x, xx$y, col = col, border
						 = border, density = density,
						angle = angle)
					return(invisible())
				}
				else return(invisible(invProj(xx)))
			}
		}
	}
	err <- F
	xx <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		Projection)
	if(!outside)
		xx <- cut.multipoly(xx, brd, in.or.out)
	if(length(xx$x) > allowed.size && plot) {
		ind <- seq(along = xx$x)
		ind <- ind[is.na(xx$x)]
		if(length(ind) == 0)
			err <- T
		else {
			ind <- c(1, ind, length(xx$x))
			if(max(diff(ind)) > allowed.size)
				err <- T
			else err <- F
		}
	}
	if(plot) {
		if(!err)
			polygon(xx$x, xx$y, col = col, border = border, density
				 = density, angle = angle)
		else print("too large polygon, change parameter allowed.zize")
	}
	if(save) {
		xx <- invProj(xx$x, xx$y, geopar$scale, geopar$b0, geopar$
			b1, geopar$l1, Projection)
		return(list(lat = xx$lat, lon = xx$lon))
	}
	return(invisible())
}

