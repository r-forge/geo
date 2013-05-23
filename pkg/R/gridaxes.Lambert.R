gridaxes.Lambert <-
function(limx, limy, scale, b0, xyratio, grid, col, reitur, smareitur, axratio,
	axlabels, b1, l1, projection, dlat, dlon, col1 = 1)
{
	lx <- c(limx[1], limx[1], limx[2], mean(limx))
	ly <- c(limy[2], limy[1], limy[2], limy[2])
	o1 <- invProj(lx, ly, scale, b0, b1, l1, projection)
	o <- invProj(limx, limy, scale, b0, b1, l1, projection)
	lines(c(o$x[1], o$x[2], o$x[2], o$x[1], o$x[1]), c(o$y[1], o$y[1],
		o$y[2], o$y[2], o$y[1]))
	r1 <- (limy[2] - limy[1])/(limx[2] - limx[1])
	# ratio
	if(dlat == 0 && dlon == 0) {
		if((o$lon[2] - o$lon[1]) > 1)
			dlon <- 1/3
		if((o$lon[2] - o$lon[1]) > 3)
			dlon <- 1/2
		if((o$lon[2] - o$lon[1]) > 6)
			dlon <- 1
		if((o$lon[2] - o$lon[1]) > 10)
			dlon <- 2
		if((o$lon[2] - o$lon[1]) > 20)
			dlon <- 4
		if((o$lon[2] - o$lon[1]) > 40)
			dlon <- 8
		if((o$lon[2] - o$lon[1]) <= 1)
			dlon <- 1/6
		if((o$lon[2] - o$lon[1]) < 0.4)
			dlon <- 1/12
		if((o$lon[2] - o$lon[1]) < 0.2)
			dlon <- 1/30
		if((o$lon[2] - o$lon[1]) < 0.1)
			dlon <- 1/60
		if((o$lon[2] - o$lon[1]) < 0.05)
			dlon <- 1/120
		dlat <- dlon/2
		if(reitur) {
			dlon <- 1
			dlat <- 0.5
		}
		if(smareitur) {
			dlon <- 0.5
			dlat <- 0.25
		}
	}
	if(dlat == 0 && dlon != 0)
		dlat <- dlon/2
	if(dlat != 0 && dlon == 0)
		dlon <- dlat * 2
	nx <- floor((o$lon[2] - o$lon[1]) * 0.3) + 2
	dlat <- dlat/axratio
	dlon <- dlon/axratio
	nlon <- floor(o1$lon[3] - o1$lon[1])/dlon + 1
	nlat <- floor(o1$lat[1] - o1$lat[2])/dlat + 1
	olo <- o1$lon[1] - ((o1$lon[1]/dlon) - floor(o1$lon[1]/dlon)) * dlon
	ola <- o1$lat[2] - ((o1$lat[2]/dlat) - floor(o1$lat[2]/dlat)) * dlat
	latgr <- ola + c(0:(nlat * 2)) * dlat
	latgr <- latgr[latgr < o1$lat[4] + dlat]
	longr <- olo + c(-1:(nlon * 2)) * dlon
	longr <- longr[longr < o1$lon[3] + dlon]
	latgr2 <- latgr
	longr2 <- longr
	nlat <- length(latgr2)
	nlon <- length(longr2)
	latgr1 <- matrix(latgr2, nlat, nlon)
	longr1 <- t(matrix(longr2, nlon, nlat))
	# 	plot grid vertical.  
	plotgr2 <- Proj(latgr1, longr1, scale, b0, b1, l1, projection)
	n <- ncol(plotgr2$x)
	n1 <- c(1:n)
	n1[1:n] <- NA
	# add NA for plot
	plx.lon <- rbind(plotgr2$x, n1)
	ply.lon <- rbind(plotgr2$y, n1)
	plx <- cut_box.1(plx.lon, ply.lon, o$x, o$y)
	if(!grid)
		plx1 <- adjust.grd(plx)
	else plx1 <- plx
	par(err = -1)
	if(grid)
		lines(plx1$x, plx1$y, col = col)
	# plot grid. 
	#	Horizontal grid
	plx$x <- matrix(plx$x, 3,  )
	n <- nrow(latgr1)
	n1 <- c(1:n)
	n1[1:n] <- NA
	# add NA for plot
	pl.lat <- rbind(t(latgr1), n1)
	pl.lon <- rbind(t(longr1), n1)
	x <- fill.points(pl.lon, pl.lat, nx = 10)
	x <- Proj(x$y, x$x, scale, b0, b1, l1, projection)
	ply <- cut_box.2(x$x, x$y, o$x, o$y)
	if(!grid)
		ply1 <- adjust.grd(ply)
	else ply1 <- ply
	par(err = -1)
	lines(ply1$x, ply1$y, col = col)
	# plot grid.
	# 	Plot axes
	indx <- c(1:length(latgr))
	indx <- indx[latgr < o1$lat[1] & latgr > o1$lat[2]]
	longr <- longr[plx$ind]
	latcha <- round((abs(latgr) - trunc(abs(latgr))) * 60, digits = 2)
	loncha <- round((abs(longr) - trunc(abs(longr))) * 60, digits = 2)
	ind1 <- c(1:length(latcha))
	ind1 <- ind1[latcha == 0]
	ind2 <- c(1:length(loncha))
	ind2 <- ind2[loncha == 0]
	indlat <- latcha == 60
	indlon <- loncha == 60
	latchar <- as.character(trunc(abs(latgr)) + indlat)
	lonchar <- as.character(trunc(abs(longr)) + indlon)
	latcha <- as.character(latcha - indlat * 60)
	loncha <- as.character(loncha - indlon * 60)
	if(length(ind1) == 0)
		latchar <- paste(latchar, "\u00b0", latcha, "'", sep = "")
	else {
		if(floor(dlat) == dlat)
			latchar[ind1] <- paste(latchar[ind1], "\u00b0")
		else latchar[ind1] <- paste(latchar[ind1], "\u00b0", "00'", sep = ""
				)
		latchar[ - ind1] <- paste(latchar[ - ind1], "\u00b0", latcha[ - ind1
			], "'", sep = "")
	}
	if(length(ind2) == 0)
		lonchar <- paste(lonchar, "\u00b0", loncha, "'", sep = "")
	else {
		if(floor(dlon) == dlon)
			lonchar[ind2] <- paste(lonchar[ind2], "\u00b0")
		else lonchar[ind2] <- paste(lonchar[ind2], "\u00b0", "00'", sep = ""
				)
		lonchar[ - ind2] <- paste(lonchar[ - ind2], "\u00b0", loncha[ - ind2
			], "'", sep = "")
	}
	par(adj = 0.5)
	if(axlabels) {
          
        # geoaxis(side=2,pos = ply$y1[indx], dis=0.3,inside=F) 
        # geoaxis(side=1,pos = plx$x[1, plx$ind],inside=F)
       # print(plx$x[1, plx$ind])
       # print(   ply$y1[indx])      
        	axis(1, plx$x[1, plx$ind], lonchar, tick = F, col = col1)
		axis(2, ply$y1[indx], latchar[indx], tick = F, col = col1,las=1) #las R vers.
	}
	return(invisible())
}

