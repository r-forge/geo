variogram <-
function(lat, lon = 0, z, nbins = 100, maxdist = 0, Hawk = T, throwout = F,
	scale = "km", evennumber = T, zzp = F, minnumber = 0, col.names = c(
	"lat", "lon"))
{
	if(is.data.frame(lat)) {
		lon <- lat[[col.names[2]]]
		lat <- lat[[col.names[1]]]
	}
	eps <- 1e-06
	rad <- 6378.388
	# Radius of earth in km.
	if(col.names[1] == "lat" && col.names[2] == "lon") {
		xy <- F
		if(length(lon) < 2) {
			lon <- lat$lon
			lat <- lat$lat
		}
		#list  
		if(scale == "nmi") rad <- rad/1.852
		# distances in miles.  
		lon <- (lon * pi)/180
		# change from degrees to radians
		lat <- (lat * pi)/180
	}
	else xy <- T
	if(throwout) {
		# throw out zero points.  
		lat <- lat[abs(z) > eps]
		lon <- lon[abs(z) > eps]
		z1 <- z[abs(z) > eps]
		z <- z1
	}
	variance <- var(z)
	count <- length(lon)
	# measurements
	if(maxdist == 0) {
		rlat <- range(lat)
		rlon <- range(lon)
		if(xy)
			maxdist <- pdistx(rlat[2], rlon[2], rlat[1], rlon[
				1]) * 0.7
		else maxdist <- pdist(rlat[2], rlon[2], rlat[1], rlon[1]) *
				0.7
		if(scale == "nmi")
			maxdist <- maxdist/1.852
	}
	varioa <- dista <- numbera <- rep(0, nbins)
	if(evennumber)
		nbins <- nbins * 10
	ddist <- maxdist/nbins
	dist <- vario <- number <- rep(0, nbins)
	dist <- .C("variogram", PACKAGE = "geo", 
		as.double(lat),
		as.double(lon),
		as.double(z),
		as.double(ddist),
		as.integer(number),
		as.double(dist),
		as.double(vario),
		as.integer(count),
		as.integer(nbins),
		as.integer(Hawk),
		as.integer(evennumber),
		as.double(varioa),
		as.double(dista),
		as.integer(numbera),
		as.integer(zzp),
		as.integer(xy))
	nbins <- dist[[9]]
	vario <- dist[[7]]
	xh <- dist[[6]]
	number <- dist[[5]]
	if(evennumber) {
		vario <- vario[1:nbins]
		xh <- xh[1:nbins]
		number <- number[1:nbins]
	}
	dist <- xh
	ind <- c(1:length(number))
	ind <- ind[number < minnumber + 1]
	if(length(ind) == 0)
		return(list(vario = vario, dist = dist, number = number, 
			variance = variance))
	else return(list(vario = vario[ - ind], dist = dist[ - ind], number = 
			number[ - ind], variance = variance))
}

