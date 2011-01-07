pointkriging <-
function(lat, lon, z, xgr, vagram, maxnumber = 16, scale = "km", option = 1,
	maxdist = 0, rat = 3, nb = 8, set = 0, areas = 0, varcalc = F, sill = 0,
	minnumber = 2, suboption = 1, outside = T, degree = 0, lognormal = F,
	zeroset = F)
{
	i <- match("rang1", names(vagram))
	if(is.na(i)) {
		if(!is.na(match("range", names(vagram))))
			vagram$rang1 <- vagram$range
		else {
			cat("variogram wrong")
			return(invisible())
		}
	}
	if(lognormal)
		varcalc <- T
	vgr <- c(vagram$rang1, vagram$sill, vagram$nugget)
	# components of variogram.  
	ndata <- length(lat)
	d <- c(1, 3, 6)
	if(degree > 2)
		degree <- 2
	#	get row indices.  
	xxx <- bua(nb)
	stdrrt <- xxx$rrt
	stdcrt <- xxx$crt
	dir <- xxx$dir
	i1 <- xxx$i1
	# 	get rid of data outside borders.  
	if(length(xgr$grpt) == 0) gr <- xgr else gr <- xgr$grpt
	if(outside) {
		m <- length(gr$lat)
		n <- length(gr$lon)
		minlat <- gr$lat[1] - nb * (gr$lat[2] - gr$lat[1])
		maxlat <- gr$lat[m] + nb * (gr$lat[m] - gr$lat[m - 1])
		minlon <- gr$lon[1] - nb * (gr$lon[2] - gr$lon[1])
		maxlon <- gr$lon[n] + nb * (gr$lon[n] - gr$lon[n - 1])
		ind <- c(1:length(lat))
		ind <- ind[lat > minlat & lat < maxlat & lon > minlon & lon <
			maxlon]
		lat <- lat[ind]
		lon <- lon[ind]
		z <- z[ind]
		ndata <- length(lat)
	}
	#	Fill up matrix of data.  
	if(length(xgr$grpt) == 0) {
		lat1 <- c(t(matrix(xgr$lat, length(xgr$lat), length(xgr$lon))))
		lon1 <- c(matrix(xgr$lon, length(xgr$lon), length(xgr$lat)))
		#		geopoints(lat1,lon1)
		n <- length(xgr$lon)
		m <- length(xgr$lat)
                #
                # # labels = FALSE added in R version. # #
                #
		row <- cut(lat, c(-999, xgr$lat, 999),labels=FALSE)
		col <- cut(lon, c(-999, xgr$lon, 999),labels=FALSE)
                inni <- rep(1, length(lat1))
	}
	#	What to set points outside the range of data to.  
	if(set == 0) mz <- 0
	if(set > 0)
		mz <- mean(z)
	if(set < 0)
		mz <- -99999
	#might be used for identification.  
	reitur <- (n + 1) * (row - 1) + col
        treitur <- rep(1, ndata)
	# storage.  
	pts.in.reit <- c(matrix(0, ndata * 1.2, 1))
	maxrt <- max(reitur)
	npts.in.reit <- rep(0, round((maxrt + 1) * 1.2))
	# 	mark points inside areas.  
	if(length(areas) > 1) {	
	ind <- c(1:length(areas$lat))
		ind <- ind[is.na(areas$lat)]
		if(length(ind) == 0)
			break()
		nareas <- length(ind) + 1
		#number of areas
		ind <- c(0, ind, (length(areas$lat) + 1))
		isub <- rep(0, length(lat))
		isub1 <- rep(0, length(lat1))
		subareas <- 1
		for(i in (1:nareas)) {
			reg <- list(lat = areas$lat[(ind[i] + 1):(ind[i + 1] -
				1)], lon = areas$lon[(ind[i] + 1):(ind[i + 1] -
				1)])
			border <- adapt(reg$lat, reg$lon)
			inn <- rep(0, length(lat))
			inn1 <- rep(0, length(lat1))
			inn <- .C("marghc",
				as.double(lon),
				as.double(lat),
				as.integer(length(lat)),
				as.double(border$lon),
				as.double(border$lat),
				as.integer(length(border$lat)),
				as.integer(border$lxv),
				as.integer(length(border$lxv)),
				as.integer(inn))
			isub <- inn[[9]] * i + isub
			inn1 <- .C("marghc",
				as.double(lon1),
				as.double(lat1),
				as.integer(length(lat1)),
				as.double(border$lon),
				as.double(border$lat),
				as.integer(length(border$lat)),
				as.integer(border$lxv),
				as.integer(length(border$lxv)),
				as.integer(inn1))
			isub1 <- inn1[[9]] * i + isub1
		}
	}
	else {
		# No special areas.  
		subareas <- 0
		isub <- rep(0, length(lat))
		isub1 <- rep(0, length(lat1))
	}
	gr$lon <- (gr$lon * pi)/180
	gr$lat <- (gr$lat * pi)/180
	lat1 <- (lat1 * pi)/180
	lon1 <- (lon1 * pi)/180
	lat <- (lat * pi)/180
	lon <- (lon * pi)/180
	if(option == 4) {
		# look for dimensions of squares. 
		if(maxdist == 0) maxdist <- vagram$rang1
		d1 <- pdist(gr$lat[1], gr$lon[1], gr$lat[2], gr$lon[2])
		d2 <- pdist(gr$lat[1], gr$lon[1], gr$lat[1], gr$lon[2])
		nm <- max(c(floor(maxdist/d1 + 1), floor(maxdist/d2 + 1)))
		if(nm > nb)
			nm <- nb
		i1 <- c(0, i1[nm + 1])
	}
	cov <- c(matrix(0, maxnumber + d[degree + 1], maxnumber + d[degree +
		1]))
	rhgtside <- x <- rhgtsbck <- rep(0, maxnumber + d[degree + 1])
	zgr <- variance <- lagrange <- rep(0, length(lat1))
	#	npts.in.reit <- rep(0, ndata)
	indrt <- jrt <- npts.in.reit
	if(varcalc && sill == 0)
		sill <- vagram$sill
	# calculate variance.  
	xy <- 0
	# not xy coordinates
	z <- .C("pointkriging",
		as.double(lat),
		as.double(lon),
		as.double(z),
		as.integer(ndata),
		as.double(lat1),
		as.double(lon1),
		as.double(zgr),
		as.integer(length(lat1)),
		as.integer(reitur),
		as.integer(n),
		as.integer(m),
		as.integer(pts.in.reit),
		as.integer(npts.in.reit),
		as.integer(maxnumber),
		as.double(vgr),
		as.integer(stdcrt),
		as.integer(stdrrt),
		as.integer(dir),
		as.integer(i1),
		as.integer(length(i1)),
		as.integer(option),
		as.integer(inni),
		as.double(cov),
		as.double(rhgtside),
		as.double(x),
		as.integer(indrt),
		as.integer(jrt),
		as.integer(maxrt),
		as.integer(treitur),
		as.double(rat),
		as.double(maxdist),
		as.double(mz),
		as.integer(isub),
		as.integer(isub1),
		as.integer(subareas),
		as.double(variance),
		as.integer(varcalc),
		as.double(rhgtsbck),
		as.double(sill),
		as.integer(minnumber),
		as.integer(suboption),
		as.integer(xy),
		as.integer(d),
		as.double(lagrange),
		as.integer(zeroset))
	zgr <- z[[7]]
	zgr[zgr == -99999] <- NA
	variance <- z[[36]]
	lagrange <- z[[44]]
	if(varcalc)
		zgr <- list(zgr = zgr, variance = variance, lagrange = lagrange
			)
	attributes(zgr)$vagram <- vagram
	attributes(zgr)$grid <- xgr
	attributes(zgr)$nb <- nb
	attributes(zgr)$option <- option
	attributes(zgr)$maxnumber <- maxnumber
	return(zgr)
}

