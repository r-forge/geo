# Parameters that can not be set.  
nonsetpar <- c("omd","oma","omi","fin","fig","mfcol","mfrow","mfg")

Elimcomp <- function(parlist){
  txt <- names(parlist)
  txt <- txt[is.na(match(txt,nonsetpar))]
  res <- list()
  for(i in txt) res[[as.character(i)]] <- parlist[[as.character(i)]]
  return(res) 
}

selectedpar <- function() 
  return(Elimcomp(par(no.readonly=T)))

geointersect <- 
function(data, border, in.or.out)
{
	tmp <- invProj(findcut(Proj(data), Proj(border), in.or.out))
	return(data.frame(lat = tmp$lat, lon = tmp$lon))
}

  
geoplot<-
function(lat = NULL, lon = 0, type = "p", pch = "*", xlim = c(0, 0), ylim = c(
	0, 0), b0 = 65, r = 1.05, country = "default", xlab = " ", ylab = " ",
	option = "cut", grid = T, new = F, cont = F,cex = 0.9,col = 1,
        lcont= c(0.13, 0.21), plotit = T, reitur = F, smareitur = F, reittext = F,
	cexrt = 0.7, csirt=NULL,axratio = 1, lwd = 0, lwd1 = 0, locator = F, axlabels = T,
	projection = "Mercator", b1 = b0, dlat = 0, dlon = 0, jitter = 0, zoom,csi=NULL,xaxdist=0.2,yaxdist=0.3
	)
{
  
  if(!is.null(csirt)) cexrt <- cexrt*csirt/0.12
  if(!is.null(csi)) cex <- cex*csi/0.12
  if(!plotit) axlabels <- F  # not plot axes if ther is no plot.  
  if(!missing(zoom)) {
    xlim <- geolocator(n = 2)
  }
	oldpar.1 <- par(no.readonly=T)
 	# first version of old parameters
	command <- sys.call()
	if((oldpar.1$fig[2] - oldpar.1$fig[1]) <= 0.6 || (oldpar.1$fig[4] -
		oldpar.1$fig[3]) <= 0.6)
		multfig <- T
	else multfig <- F
	if(projection == "none") {
                if(is.list(xlim) &&  any(!is.na(match(c("x","y"),names(xlim))))){
                  ylim <- xlim$y
                  xlim <- xlim$x
		}
	}
	else {
                if(is.list(xlim) &&  any(!is.na(match(c("lat","lon"),names(xlim))))){
                  ylim <- xlim$lat
                  xlim <- xlim$lon
		}
	}
	if(is.null(lat) && xlim[2] == xlim[1] && ylim[2] == ylim[1] && !locator
		) {
		#std plot
		if(!multfig) par(fig = geopar.std$fig)
		if(!multfig)
			par(plt = geopar.std$plt)
		xlim <- geopar.std$xlim
		ylim <- geopar.std$ylim
		if(!multfig)
			par(mex = geopar.std$mex)
	}
	if(is.null(lat)) {
		lat <- c(65, 66)
		lon <- c(-28, -27)
		type <- "n"
	}
	oldpar <- selectedpar()
	if(locator & missing(zoom)) {
		limits <- geolocator(n = 2)
		if(geopar$projection == "none") {
			xlim <- limits$x
			ylim <- limits$y
		}
		else {
			xlim <- limits$lon
			ylim <- limits$lat
		}
	}
	xlim <- sort(xlim)
	ylim <- c(ylim)
	if(projection == "none") {
		if(length(country) == 1)
			if(country == "default")
				country <- "none"
	}
	else {
		if(length(country) == 1)
                  if(country == "default")
                    eval(parse(text=paste("country <- ",COUNTRY.DEFAULT)))
	}
	init(lat, lon = lon, type = type, pch = pch, xlim = xlim, ylim = ylim,
		b0 = b0, r = r, country = country, xlab = xlab, ylab = ylab,
		option = option, grid = grid, new = new, cont = cont, cex = cex,
		col = col, lcont = lcont, plotit = plotit, reitur = reitur,
		smareitur = smareitur, reittext = reittext, axratio = axratio,
		lwd = lwd, axlabels = axlabels, oldpar = oldpar, projection = 
		projection, b1 = b1, dlat = dlat, dlon = dlon, command = 
		command, jitter = jitter,xaxdist=xaxdist,yaxdist=yaxdist)
        oldpar.1 <- Elimcomp(oldpar.1)
	par(oldpar.1)
#        par(new=T)
	if(reittext)
		plot.reitnr(cexrt, lwd = lwd)
	# number of squares
	if(length(country) > 1 && plotit) geolines(country, col = col, lwd = 
			lwd1)
	# plot country
	return(invisible())
}

init<-
function(lat, lon = 0, type = "p", pch = "*", xlim = c(0, 0), ylim = c(0, 0),
	b0 = 65, r = 1.05, country = iceland, xlab = "Longitude", ylab = 
	"Latitude", option = "cut", grid = T, new = F, cont = F, cex =0.7,
	col = 1, lcont = c(0.13, 0.21), plotit = T, reitur = F, smareitur = F,
	reittext = F, axratio = 1, lwd = 0, axlabels = T, oldpar, projection = 
	"Mercator", b1 = 65, dlat = 0, dlon = 0, command = 0, jitter = 0,xaxdist,yaxdist)
{
	if(exists("geopar"))
		geopar <- geopar
	else geopar <- list()
  
	if(projection == "none") {
		if(length(lon) == 1) {
			lon <- lat$y
			lat <- lat$x
		}
	}
	else {
		if(length(lon) == 1) {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	nlat <- length(lat)
	lat <- lat + (runif(nlat) - 0.5) * jitter
	lon <- lon + (runif(nlat) - 0.5) * jitter
	if(xlim[1] == xlim[2])
		l1 <- mean(range(lon[!is.na(lon)]))
	else l1 <- mean(xlim)
	par(xpd = F)
	scale <- "km"
	xgr <- Proj(lat, lon, scale, b0, b1, l1, projection)
	# 	size of text
	par(cex = cex)
	if(lwd != 0)
		par(lwd = lwd)
	if(!axlabels) {
		xlab <- ""
		ylab <- ""
	}
	# 	contourplot
	if(!cont) {
		lcont[1] <- 0
		lcont[2] <- 0
	}
	if(cont)
		option <- "nocut"
	plt <- oldpar$plt
	contlab <- plt
	contlines <- plt
	contlines[1] <- plt[1] + lcont[2] * (plt[2] - plt[1])
	contlab[2] <- plt[1] + lcont[1] * (plt[2] - plt[1])
	par(plt = contlines)
	if(cont)
		geopar$cont <- T
	else geopar$cont <- F
	# Find borders, adjust them if given.  
	xyratio <- par()$pin[1]/par()$pin[2]
	#*1.04777  ratio of axes.
	if(projection == "none") {
		ind <- c(1:length(xgr$x))
		ind <- ind[!is.na(xgr$x)]
		#No NAs
		if(xlim[1] == xlim[2]) {
			xmin <- min(xgr$x[ind])
			xmax <- max(xgr$x[ind])
		}
		else {
			xmin <- xlim[1]
			xmax <- xlim[2]
			r <- 1
		}
		if(ylim[1] == ylim[2]) {
			ymin <- min(xgr$y[ind])
			ymax <- max(xgr$y[ind])
		}
		else {
			ymin <- ylim[1]
			ymax <- ylim[2]
			r <- 1
		}
	}
	else {
		ind <- c(1:length(xgr$lon))
		ind <- ind[!is.na(xgr$lon)]
		#No NAs
		if(xlim[1] == xlim[2]) {
			xmin <- min(xgr$lon[ind])
			xmax <- max(xgr$lon[ind])
		}
		else {
			xmin <- xlim[1]
			xmax <- xlim[2]
			r <- 1
		}
		if(ylim[1] == ylim[2]) {
			ymin <- min(xgr$lat[ind])
			ymax <- max(xgr$lat[ind])
		}
		else {
			ymin <- ylim[1]
			ymax <- ylim[2]
			r <- 1
		}
	}
	if(projection == "Lambert") {
		xt1 <- c(l1, xmin, xmax, xmax)
		xt2 <- c(ymin, ymin, ymin, ymax)
	}
	else if(projection == "none") {
		xt2 <- c(xmin, xmax)
		xt1 <- c(ymin, ymax)
	}
	else {
		xt1 <- c(xmin, xmax)
		xt2 <- c(ymin, ymax)
	}
	xl <- Proj(xt2, xt1, scale, b0, b1, l1, projection)
	xmin <- min(xl$x)
	ymin <- min(xl$y)
	xmax <- max(xl$x)
	ymax <- max(xl$y)
	xymax <- max((ymax - ymin), (xmax - xmin)/xyratio)
	meanx <- (xmin + xmax)/2
	meany <- (ymin + ymax)/2
	r1 <- r + (r - 1)/2
	r1 <- r1 - 0.5
	if(option == "cut") {
		# cut figure and graph region 
		limx <- c(meanx - r1 * (xmax - xmin), meanx + r1 * (xmax - xmin
			))
		limy <- c(meany - r1 * (ymax - ymin), meany + r1 * (ymax - ymin
			))
		xyr <- (ymax - ymin)/((xmax - xmin)/xyratio)
		pinpar <- c(1:2)
		if(xyr > 1) {
			pinpar[1] <- par()$pin[1]/xyr
			pinpar[2] <- par()$pin[2]
		}
		else {
			pinpar[1] <- par()$pin[1]
			pinpar[2] <- par()$pin[2] * xyr
		}
		par(pin = pinpar)
	}
	else {
		limx <- c(meanx - r1 * xymax * xyratio, meanx + r1 * xymax *
			xyratio)
		limy <- c(meany - r1 * xymax, meany + r1 * xymax)
	}
	if(type == "l") {
		gx <- limx
		gy <- limy
		border <- list(x = c(gx[1], gx[2], gx[2], gx[1], gx[1]), y = c(
			gy[1], gy[1], gy[2], gy[2], gy[1]))
		xx <- findline(xgr, border)
	}
	else {
		ind <- c(1:length(xgr$x))
		ind <- ind[(xgr$x > limx[1]) & (xgr$x < limx[2]) & (xgr$y >
			limy[1]) & (xgr$y < limy[2])]
		xx <- list(x = xgr$x[ind], y = xgr$y[ind])
	}
	if(length(xx$x) == 0) {
		type <- "n"
		xx <- xgr
	}
	# to get rid of errors if no point in plot. 
	par(new = new)
	if(plotit) {
		if(projection == "none") {
			plot(xx$x, xx$y, type = type, pch = pch, xlim = limx,
				ylim = limy, xlab = xlab, ylab = ylab, col = 
				col)
		}
		else {
			plot(xx$x, xx$y, type = type, pch = pch, xlim = limx,
				ylim = limy, axes = FALSE, xlab = xlab, ylab = ylab,
				col = col)
			# plot grid and axes
			if(projection == "Lambert"){
                         d <- gridaxes.Lambert(limx,limy, scale, b0, xyratio, grid, col,
					   reitur, smareitur, axratio, axlabels,
					   b1, l1, projection, dlat, dlon)
                        }
                        else{ 

                          d <- gridaxes(limx, limy, scale, b0, xyratio,
				        grid, col, reitur, smareitur, axratio,
				        axlabels, b1, l1, projection, dlat,
				        dlon)
                        }
               }
	}
	else plot(xx$x, xx$y, type = "n", pch = pch, xlim = limx, ylim = limy,
			axes = F, xlab = "", ylab = "", col = col)
        #par(new = T)
	gpar <- par(no.readonly = TRUE)
	# save graphical setup
	o <- invProj(limx, limy, scale, b0, b1, l1, projection)
        gpar <- Elimcomp(gpar)
	geopar <- list(gpar = gpar, limx = limx, limy = limy, scale = scale,
		b0 = b0, b1 = b1, l1 = l1, contlab = contlab, contlines = 
		contlines, cont = cont, projection = projection, origin = o,
		command = command)
        
        # assign, pos=1 added in R version.
  	assign("geopar", geopar, pos=1,immediate = F)
        
        # Extra to get geoaxis instead of normal axis added in R version.
        
         if(axlabels && projection == "Mercator"){
           if(!reitur && !smareitur){
             geoaxis(side=2,dist=yaxdist,dlat=d$dlat,inside=F,cex=cex)
             geoaxis(side=1,dlon=d$dlon,inside=F,cex=cex,dist=xaxdist)
           }
           else{
             if(reitur){
             geoaxis(side=2,dlat=d$dlat,inside=F,cex=0.63)
             geoaxis(side=1,dlon=d$dlon,inside=F,cex=0.63)
             }
             if(smareitur){
             geoaxis(side=2,dlat=d$dlat*2,inside=F,cex=0.63)
             geoaxis(side=1,dlon=d$dlon*2,inside=F,cex=0.63)
             }
           }
           
         }
	return(invisible())
        
}

Proj<-
function(a, b = 0, scale = geopar$scale, b0 = geopar$b0, b1 = geopar$b1, l1 = 
	geopar$l1, projection = geopar$projection, col.names = c("lon", "lat")
	)
{
	if(col.names[1] != "lon" || col.names[2] != "lat")
		projection <- "none"
	if(is.list(a)) {
		if(projection == "none") {
			b <- a$y
			a <- a$x
		}
		else {
			b <- a$lon
			a <- a$lat
		}
	}
	if(projection == "Lambert") {
		x <- lambert(a, b, b0, l1, b1, scale, old = T)
	}
	else if(projection == "Mercator") {
		x <- mercator(a, b, scale, b0)
	}
	else if(projection == "none") {
		x <- list(x = a, y = b)
	}
}

invProj<-
function(x, y = NULL, scale = geopar$scale, b0 = geopar$b0, b1 = geopar$b1,
	l1 = geopar$l1, projection = geopar$projection)
{
	if(is.null(y)) {
		y <- x$y
		x <- x$x
	}
	if(projection == "Lambert") {
		x <- invlambert(x, y, b0, l1, b1, scale, old = T)
	}
	else if(projection == "Mercator") {
		x <- invmerc(x, y, scale, b0)
	}
	else if(projection == "none") {
		x <- list(x = x, y = y)
	}
}


mercator<-
function(lat, lon, scale = "km", b0 = 65)
{
	radius <- 6378.388
	m.p.km <- 1.852
	mult <- radius
	if(scale != "km")
		mult <- mult/m.p.km
	l1 <- (lon * pi)/180
	b1 <- (lat * pi)/180
	b0 <- (b0 * pi)/180
	x <- mult * cos(b0) * l1
	y <- mult * cos(b0) * log((1 + sin(b1))/cos(b1))
	return(invisible(list(lat = lat, lon = lon, x = x, y = y, scale = scale,
		projection = "mercator", b0 = b0, L0 = NULL)))
}

lambert<-
function(lat, lon, lat0, lon0, lat1, scale = "km", old = F)
{
	a <- 6378.388
	# radius at equator
	e <- sqrt(2/297 - (1/297)^2)
	# eccensitret.
	lat11 <- lat1
	# temporary storage
	# 	change to radians  
	lat1 <- (lat1 * pi)/180
	lat0 <- (lat0 * pi)/180
	lon0 <- (lon0 * pi)/180
	lat <- (lat * pi)/180
	lon <- (lon * pi)/180
	#	one or two touching points.  
	if(length(lat1) == 2) {
		lat2 <- lat1[2]
		lat1 <- lat1[1]
		np <- 2
	}
	else np <- 1
	m1 <- cos(lat1)/sqrt(1 - e * e * (sin(lat1))^2)
	if(old) {
		t1 <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat1)))
		t0 <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat0)))
	}
	else {
		t1 <- tan(pi/4 - lat1/2)/((1 - e * sin(lat1))/(1 + e * sin(
			lat1)))^(e/2)
		t0 <- tan(pi/4 - lat0/2)/((1 - e * sin(lat0))/(1 + e * sin(
			lat0)))^(e/2)
	}
	# one tangent.   
	if(np == 1) n <- sin(lat1) else {
		m2 <- cos(lat2)/(1 - e * e * (sin(lat2))^2)
		if(old)
			t2 <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat2)))
		else t2 <- tan(pi/4 - lat2/2)/((1 - e * sin(lat2))/(1 + e *
				sin(lat2)))^(e/2)
		n <- (ln(m1) - ln(m2))/(ln(t1) - ln(t2))
	}
	F1 <- m1/(n * t1^n)
	p0 <- a * F1 * t0^n
	if(old)
		t <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat)))
	else t <- tan(pi/4 - lat/2)/((1 - e * sin(lat))/(1 + e * sin(lat)))^
			(e/2)
	p <- a * F1 * t^n
	theta <- n * (lon - lon0)
	x <- p * sin(theta)
	y <- p0 - p * cos(theta)
	return(invisible(list(lat = (lat * 180)/pi, lon = (lon * 180)/pi, x = x,
		y = y, scale = scale, projection = "Lambert", lat0 = (lat0 *
		180)/pi, lon0 = (lon0 * 180)/pi, lat1 = lat11)))
}

invmerc<-
function(x, y, scale = "km", b0 = 65)
{
	radius <- 6378.388
	m.p.km <- 1.852
	mult <- radius
	if(scale != "km")
		mult <- mult/m.p.km
	b0 <- (b0 * pi)/180
	lon <- (x/(mult * cos(b0)) * 180)/pi
	# Have to find latitude by iteration.
	c1 <- exp(y/(mult * cos(b0)))
	lat1 <- c(1:length(y))
	lat1[1:length(y)] <- b0
	# initial guess
	lat <- c(1:length(y))
	ind <- c(1:length(y))
	#index.
        ind <- ind[!is.na(c1)]
	# NA dont work in sum.
	while(sum(abs(lat1[ind] - lat[ind]))/sum(abs(lat[ind])) > 1e-07) {
		lat <- lat1
		lat1 <- lat - ((1 + sin(lat))/cos(lat) - c1)/((1 + sin(lat))/
			(cos(lat)^2))
	}
	lat <- lat1
	lat <- (lat * 180)/pi
	return(invisible(list(lat = lat, lon = lon, x = x, y = y, scale = scale,
		projection = "mercator", b0 = b0, L0 = NULL)))
}

invlambert<-
function(x, y, lat0, lon0, lat1, scale = "km", old = F)
{
	a <- 6378.388
	# radius at equator
	e <- sqrt(2/297 - (1/297)^2)
	# eccensitret.
	lat11 <- lat1
	# temporary storage
	# 	change to radians  
	lat1 <- (lat1 * pi)/180
	lat0 <- (lat0 * pi)/180
	lon0 <- (lon0 * pi)/180
	#	one or two touching points.  
	if(length(lat1) == 2) {
		lat2 <- lat1[2]
		lat1 <- lat1[1]
		np <- 2
	}
	else np <- 1
	m1 <- cos(lat1)/sqrt(1 - e * e * (sin(lat1))^2)
	if(old) {
		t1 <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat1)))
		t0 <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat0)))
	}
	else {
		t1 <- tan(pi/4 - lat1/2)/((1 - e * sin(lat1))/(1 + e * sin(
			lat1)))^(e/2)
		t0 <- tan(pi/4 - lat0/2)/((1 - e * sin(lat0))/(1 + e * sin(
			lat0)))^(e/2)
	}
	# one tangent.   
	if(np == 1) n <- sin(lat1) else {
		m2 <- cos(lat2)/(1 - e * e * (sin(lat2))^2)
		if(old)
			t2 <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat2)))
		else t2 <- tan(pi/4 - lat2/2)/((1 - e * sin(lat2))/(1 + e *
				sin(lat2)))^(e/2)
		n <- (ln(m1) - ln(m2))/(ln(t1) - ln(t2))
	}
	F1 <- m1/(n * t1^n)
	p0 <- a * F1 * t0^n
	p <- sign(n) * sqrt(x^2 + (p0 - y)^2)
	theta <- atan(x/(p0 - y))
	t <- (p/(a * F1))^(1/n)
	lon <- theta/n + lon0
	lat <- pi/2 - 2 * atan(t)
	for(i in 1:2) {
		# very rapid convergence in all cases.  
		lat <- pi/2 - 2 * atan(t * ((1 - e * sin(lat))/(1 + e * sin(
			lat)))^(e/2))
	}
	return(invisible(list(lat = (lat * 180)/pi, lon = (lon * 180)/pi, x = x,
		y = y, scale = scale, projection = "lambert", lat0 = (lat0 *
		180)/pi, lon0 = (lon0 * 180)/pi, lat1 = lat11)))
}




plot.reitnr<-
function(cexrt, lwd = 0)
{
	lat <- invProj(geopar$limx, geopar$limy, geopar$scale, geopar$b0, 
		geopar$b1, geopar$l1, geopar$projection)
	minlat <- floor(lat$lat[1] * 2)/2 - 0.5
	minlon <- floor(lat$lon[1]) - 1
	maxlon <- floor(lat$lon[2]) + 1
	maxlat <- floor(lat$lat[2] * 2)/2 + 0.5
	nlat <- (maxlat - minlat) * 2 + 1
	nlon <- (maxlon - minlon) + 1
	lon <- minlon + c(0:nlon)
	lat <- minlat + c(0:nlat) * 0.5
	lon <- lon + 0.5
	lat <- lat + 0.25
	nlat <- length(lat)
	nlon <- length(lon)
	lat <- c(matrix(lat, nlat, nlon))
	lon <- c(t(matrix(lon, nlon, nlat)))
	z <- d2r(lat, lon)
	geotext(lat, lon, z, cex = cexrt, lwd = lwd)
}

geotows <- 
function(lat, lon, lat1, lon1, col=1,col.names = c("kastad.n.breidd", 
	"kastad.v.lengd", "hift.n.breidd", "hift.v.lengd"), ...)
{
	if(is.data.frame(lat) && missing(lat1)) {
		lat1 <- lat[, col.names[3]]
		lon1 <- lat[, col.names[4]]
		lon <- lat[, col.names[2]]
		lat <- lat[, col.names[1]]
	}
	if(is.data.frame(lat) && !missing(lat1)) {
		lon <- lat$lon
		lat <- lat$lat
		lon1 <- lat1$lon
		lat1 <- lat1$lat
	}
	lat <- matrix(lat, length(lat), 3)
	lat[, 2] <- lat1
	lat[, 3] <- NA
	lat <- c(t(lat))
	lon <- matrix(lon, length(lon), 3)
	lon[, 2] <- lon1
	lon[, 3] <- NA
	lon <- c(t(lon))
	geolines(lat, lon,col=col, ...)
	return(invisible())
}


d2r<-
function(lat, lon = NULL)
{
	if(is.null(lon)) {
		lon <- lat$lon
		lat <- lat$lat
	}
	lat <- lat + 1e-06
	lon <- lon - 1e-06
	lon <-  - lon
	reit <- (floor(lat) - 60) * 100 + floor(lon)
	reit <- ifelse(lat - floor(lat) > 0.5, reit + 50, reit)
	return(reit)
}
gridaxes<-
function(limx, limy, scale, b0, xyratio, grid, col, reitur, smareitur, axratio,
	axlabels, b1, l1, projection, dlat, dlon)
{
        axlabels=F        # added to make geoaxis default in init() for R ver.
	o <- invProj(limx, limy, scale, b0, b1, l1, projection)
	r1 <- (limy[2] - limy[1])/(limx[2] - limx[1])
	# ratio
	nlon <- 30
	nlat <- round((nlon * r1)/xyratio) * 2
	if(dlat == 0 && dlon == 0) {
		if((o$lon[2] - o$lon[1]) > 40)
			dlon <- 10
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
	dlat <- dlat/axratio
	dlon <- dlon/axratio
	olo <- o$lon[1] - ((o$lon[1]/dlon) - floor(o$lon[1]/dlon)) * dlon
	ola <- o$lat[1] - ((o$lat[1]/dlat) - floor(o$lat[1]/dlat)) * dlat
	latgr <- ola + c(0:(nlat * 2)) * dlat
	latgr[latgr > 85] <- 85
	longr <- olo + c(0:(nlon * 2)) * dlon
	latgr <- latgr[(latgr <= o$lat[2]) & (latgr > o$lat[1])]
	#171
	longr <- longr[(longr <= o$lon[2]) & (longr > o$lon[1])]
	latgr2 <- c(o$lat[1], latgr, o$lat[2])
	longr2 <- c(o$lon[1], longr, o$lon[2])
	nlat <- length(latgr2)
	nlon <- length(longr2)
	latgr1 <- matrix(latgr2, nlat, nlon)
	longr1 <- t(matrix(longr2, nlon, nlat))
	# 	plot grid
	plotgr2 <- Proj(latgr1, longr1, scale, b0, b1, l1, projection)
	n <- ncol(plotgr2$x)
	n1 <- c(1:n)
	n1[1:n] <- NA
	# add NA for plot
	plx.lon <- rbind(plotgr2$x, n1)
	ply.lon <- rbind(plotgr2$y, n1)
	par(err = -1)
	if(grid)
		lines(plx.lon, ply.lon, col = col)
	# plot grid. 
	n <- nrow(plotgr2$x)
	n1 <- c(1:n)
	n1[1:n] <- NA
	# add NA for plot
	plx.lat <- rbind(t(plotgr2$x), n1)
	ply.lat <- rbind(t(plotgr2$y), n1)
	par(err = -1)
	if(grid)
		lines(plx.lat, ply.lat, col = col)
	# plot grid.
	# 	Plot axes
	latcha <- round((abs(latgr) - trunc(abs(latgr))) * 60, digits = 2)
	loncha <- round((abs(longr) - trunc(abs(longr))) * 60, digits = 2)
	indlat <- latcha == 60
	indlon <- loncha == 60
	latchar <- as.character(trunc(abs(latgr)) + indlat)
	lonchar <- as.character(trunc(abs(longr)) + indlon)
	latcha <- as.character(latcha - indlat * 60)
	loncha <- as.character(loncha - indlon * 60)
	latmin <- rep("'", length(latcha))
	lonmin <- rep("'", length(loncha))
	if(floor(dlat) == dlat) {
		ind <- c(1:length(latcha))
		ind <- ind[latcha == "0"]
		latcha[ind] <- " "
		latmin[ind] <- " "
	}
	else latcha[latcha == "0"] <- "00"
	if(floor(dlon) == dlon) {
		ind <- c(1:length(loncha))
		ind <- ind[loncha == "0"]
		loncha[ind] <- " "
		lonmin[ind] <- " "
	}
	else loncha[loncha == "0"] <- "00"
	latchar <- paste(latchar, "", latcha, latmin, sep = "")
	lonchar <- paste(lonchar, "", loncha, lonmin, sep = "")
	latchar <- c(" ", latchar, " ")
	lonchar <- c(" ", lonchar, " ")
	#	vect<-c(1:length(longr2)); vect[1:length(longr2)] <- o$y[1] 
	vect <- rep(60, length(longr2))
	# bretting 11-7
	plotgrlon <- Proj(vect, longr2, scale, b0, b1, l1, projection)
	vect <- c(1:length(latgr2))
	vect[1:length(latgr2)] <- o$x[1]
	plotgrlat <- mercator(latgr2, vect, scale, b0)
	par(adj = 0.5)
	if(axlabels) {
		if(grid) {
			# how the axes are plotted. 
                        axis(1, plotgrlon$x, lonchar, tick = F, col = col) # If grid.
                        axis(2, plotgrlat$y, latchar, tick = F, col = col)
		}
                else {
			axis(1, plotgrlon$x, tick = F, col = col) # If axlabels.
			axis(3, plotgrlon$x, F, tick = F, col = col)
			axis(2, plotgrlat$y, latchar, tick = F, col = col)
			axis(4, plotgrlat$y, F, tick = F, col = col)
			xgr <- Proj(latgr, longr, scale, b0, b1, l1, projection
				)
			plot.nogrid(o, xgr$x, xgr$y, col)
		}
	}
		# no axlabels
		if(grid) {
			# how the axes are plotted.
                        axis(1, plotgrlon$x, F, tick = F, col = col)
			axis(2, plotgrlat$y, F, tick = F, col = col)
		}
		else {
			xgr <- Proj(latgr, longr, scale, b0, b1, l1, projection)
			plot.nogrid(o, xgr$x, xgr$y, col)
		}

	return(list(dlon=dlon,dlat=dlat))
}
gridaxes.Lambert<-
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
	plx <- cut.box.1(plx.lon, ply.lon, o$x, o$y)
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
	ply <- cut.box.2(x$x, x$y, o$x, o$y)
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
		latchar <- paste(latchar, "", latcha, "'", sep = "")
	else {
		if(floor(dlat) == dlat)
			latchar[ind1] <- paste(latchar[ind1], "")
		else latchar[ind1] <- paste(latchar[ind1], "", "00'", sep = ""
				)
		latchar[ - ind1] <- paste(latchar[ - ind1], "", latcha[ - ind1
			], "'", sep = "")
	}
	if(length(ind2) == 0)
		lonchar <- paste(lonchar, "", loncha, "'", sep = "")
	else {
		if(floor(dlon) == dlon)
			lonchar[ind2] <- paste(lonchar[ind2], "")
		else lonchar[ind2] <- paste(lonchar[ind2], "", "00'", sep = ""
				)
		lonchar[ - ind2] <- paste(lonchar[ - ind2], "", loncha[ - ind2
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

cut.box.1<-
function(x, y, xb, yb)
{
	ind <- c(1:length(x))
	ind <- ind[is.na(x)]
	x <- matrix(x,  , ind[1], byrow = T)
	y <- matrix(y,  , ind[1], byrow = T)
	n <- ind[1] - 1
	t1 <- (yb[1] - y[, 1])/(y[, n] - y[, 1])
	t2 <- (yb[2] - y[, 1])/(y[, n] - y[, 1])
	x1 <- y1 <- matrix(NA, nrow(x), 3)
	x1[, 1] <- x[, 1] + t1 * (x[, n] - x[, 1])
	x1[, 2] <- x[, 1] + t2 * (x[, n] - x[, 1])
	y1[, 1] <- y[, 1] + t1 * (y[, n] - y[, 1])
	y1[, 2] <- y[, 1] + t2 * (y[, n] - y[, 1])
	ind2 <- cut(x1[, 1], xb,labels=FALSE) # labels=FALSE R ver.
	ind <- c(1:length(ind2))
	ind2 <- ind[!is.na(ind2)]
	ind <- cut(x1[, 1], c(-9999999, xb),labels=FALSE) # labels=FALSE R ver.
	ind1 <- c(1:length(ind))
	ind1 <- ind1[!is.na(ind) & ind == 1]
	t1 <- (xb[1] - x[ind1, 1])/(x[ind1, n] - x[ind1, 1])
	x1[ind1, 1] <- x[ind1, 1] + t1 * (x[ind1, n] - x[ind1, 1])
	y1[ind1, 1] <- y[ind1, 1] + t1 * (y[ind1, n] - y[ind1, 1])
	ind1 <- c(1:length(ind))
	ind1 <- ind1[is.na(ind)]
	t1 <- (xb[2] - x[ind1, 1])/(x[ind1, n] - x[ind1, 1])
	x1[ind1, 1] <- x[ind1, 1] + t1 * (x[ind1, n] - x[ind1, 1])
	y1[ind1, 1] <- y[ind1, 1] + t1 * (y[ind1, n] - y[ind1, 1])
	ind <- cut(x1[, 2], c(-9999999, xb),labels=FALSE )# labels=FALSE R ver.
	ind1 <- c(1:length(ind))
	ind1 <- ind1[ind == 1 | is.na(ind)]
	x1[ind1,  ] <- NA
	y1[ind1,  ] <- NA
	return(list(x = t(x1), y = t(y1), ind = ind2))
}
fill.points<-
function(x, y, nx, option = 1)
{
	n <- length(x)
	ny <- nx
	if(option != 1) {
		naind <- c(1:length(x))
		naind <- naind[is.na(x)]
	}
	dx <- (x[2:n] - x[1:(n - 1)])/(ny)
	dy <- (y[2:n] - y[1:(n - 1)])/(ny)
	x1 <- matrix(x[1:(n - 1)], n - 1, nx)
	y1 <- matrix(y[1:(n - 1)], n - 1, nx)
	ind <- c(0:(nx - 1))
	ind <- matrix(ind, n - 1, nx, byrow = T)
	dx <- matrix(dx, n - 1, nx)
	dy <- matrix(dy, n - 1, nx)
	x1 <- t(x1 + ind * dx)
	y1 <- t(y1 + ind * dy)
	ind <- c(1:length(y1))
	ind <- ind[is.na(y1) & row(y1) != 1]
	if(length(ind) != 0) {
		x1 <- x1[ - ind]
		y1 <- y1[ - ind]
	}
	if(is.na(x1[length(x1)])) {
		x1 <- c(x1, NA)
		y1 <- c(y1, NA)
	}
	ind <- c(1:length(x1))
	ind <- ind[is.na(x1)]
	if(length(ind) > 0) {
		ind <- matrix(ind,  , 2, byrow = T)
		if(option == 1) {
			ind <- ind[, 1]
			x1 <- x1[ - ind]
			y1 <- y1[ - ind]
		}
		else {
			ind <- ind[, 1]
			x1[ind] <- x[naind - 1]
			y1[ind] <- y[naind - 1]
		}
	}
	if(option != 1) {
		x1 <- c(x1, x[n])
		y1 <- c(y1, y[n])
	}
	return(list(x = x1, y = y1))
}
cut.box.2<-
function(x, y, xb, yb)
{
	ind <- c(1:length(x))
	inds <- ind[is.na(x)]
	ind <- inds
	xx <- matrix(x,  , ind[1], byrow = T)
	yy <- matrix(y,  , ind[1], byrow = T)
	ind <- cut(x, xb,labels=FALSE) # labels=FALSE R ver.
	ind1 <- ind[2:length(ind)]
	ind <- ind[1:(length(ind) - 1)]
	ii <- c(1:length(ind))
	i <- ifelse(is.na(ind) & !is.na(ind1), ii, NA)
	i <- i[!is.na(i)]
	i1 <- ifelse(!is.na(ind) & is.na(ind1), ii, NA)
	i1 <- i1[!is.na(i1)]
	i2 <- c(1:length(ind))
	i2 <- i2[is.na(ind)]
	x2 <- y2 <- x3 <- y3 <- matrix(NA, nrow(xx), 3)
	x2[, 1] <- x[i]
	x2[, 2] <- x[i + 1]
	y2[, 1] <- y[i]
	y2[, 2] <- y[i + 1]
	x3[, 1] <- x[i1]
	x3[, 2] <- x[i1 + 1]
	y3[, 1] <- y[i1]
	y3[, 2] <- y[i1 + 1]
	t1 <- (xb[1] - x2[, 1])/(x2[, 2] - x2[, 1])
	t2 <- (xb[2] - x3[, 1])/(x3[, 2] - x3[, 1])
	x1 <- y1 <- matrix(NA, nrow(xx), 3)
	x1[, 1] <- x2[, 1] + t1 * (x2[, 2] - x2[, 1])
	x1[, 2] <- x3[, 1] + t2 * (x3[, 2] - x3[, 1])
	y1[, 1] <- y2[, 1] + t1 * (y2[, 2] - y2[, 1])
	y1[, 2] <- y3[, 1] + t2 * (y3[, 2] - y3[, 1])
	y[i2] <- NA
	x[i2] <- NA
	x[i] <- x1[, 1]
	y[i] <- y1[, 1]
	x[i1 + 1] <- x1[, 2]
	y[i1 + 1] <- y1[, 2]
	ind <- cut(y, c(-999999, yb, 999999),labels=FALSE) # labels=FALSE R ver.
	ind1 <- c(1:length(ind))
	ind1 <- ind1[ind != 2]
	x[ind1] <- NA
	y[ind1] <- NA
	return(list(x = x, y = y, x1 = c(x1[, 1]), y1 = c(y1[, 1])))
}
adjust.grd<-
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
plot.nogrid<-
function(o, xgr, ygr, col)
{
	frame <- list(x = c(o$x[1], o$x[2], o$x[2], o$x[1], o$x[1]), y = c(
		o$y[1], o$y[1], o$y[2], o$y[2], o$y[1]))
	dx <- (o$x[2] - o$x[1])/100
	ly <- length(ygr)
	lx <- length(xgr)
	lengd <- ly * 2 + lx * 2
	o1 <- o$x[1]
	ind <- c(1:ly)
	my <- mx <- matrix(NA, lengd, 3)
	mx[ind, 1] <- o1
	mx[ind, 2] <- o1 + dx
	my[ind, 1] <- my[ind, 2] <- ygr
	o1 <- o$x[2]
	ind <- c((ly + 1):(ly * 2))
	mx[ind, 1] <- o1 - dx
	mx[ind, 2] <- o1
	my[ind, 1] <- my[ind, 2] <- ygr
	o1 <- o$y[1]
	ind <- c((ly * 2 + 1):(ly * 2 + lx))
	my[ind, 1] <- o1
	my[ind, 2] <- o1 + dx
	mx[ind, 1] <- mx[ind, 2] <- xgr
	o1 <- o$y[2]
	ind <- c((ly * 2 + lx + 1):(ly * 2 + lx * 2))
	my[ind, 1] <- o1 - dx
	my[ind, 2] <- o1
	mx[ind, 1] <- mx[ind, 2] <- xgr
	lines(t(mx), t(my), col = col)
	lines(frame, col = col)
	return(invisible())
}
geolines<-
function(lat, lon = 0, col = 1, lwd = 0, lty = 0, nx = 1, outside = F, 
	return.data = F)
{
	if(length(lon) == 1) {
		# For polygon structures.
		if(!is.null(lat$length)) n <- lat$length else n <- max(c(length(
				lat$y), length(lat$lat)))
		if(geopar$projection == "none") {
			lon <- lat$y[1:n]
			lat <- lat$x[1:n]
		}
		else {
			lon <- lat$lon[1:n]
			lat <- lat$lat[1:n]
		}
	}
	if(geopar$projection != "none") {
		# degrees and minutes
		if(mean(lat, na.rm = T) > 1000) {
			lat <- geoconvert(lat)
			lon <-  - geoconvert(lon)
		}
	}
	if(outside)
		par(xpd = T)
	else par(xpd = F)
	if(nx > 1) {
		# fill in with points for lambert. 
		x <- fill.points(lat, lon, nx, option = 2)
		lat <- x$x
		lon <- x$y
	}
	oldpar <- selectedpar()
	par(geopar$gpar)
	if(lwd != 0)
		par(lwd = lwd)
	if(lty != 0)
		par(lty = lty)
	on.exit(par(oldpar))
	xx <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection)
	if(!outside) {
		gx <- geopar$limx
		gy <- geopar$limy
		border <- list(x = c(gx[1], gx[2], gx[2], gx[1], gx[1]), y = c(
			gy[1], gy[1], gy[2], gy[2], gy[1]))
		xx <- findline(xx, border)
	}
	else par(xpd = F)
	#c program not used
	lines(xx$x, xx$y, col = col)
	par(oldpar)
	if(return.data) {
		xx <- invProj(xx)
		xx <- data.frame(lat = xx$lat, lon = xx$lon)
		return(invisible(xx))
	}
	else return(invisible())
}

findline<-
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
prepare.line<-
function(x)
{
	n <- length(x)
	x1 <- x[2:n]
	x2 <- x[1:(n - 1)]
	ind <- c(1:(n - 1))
	ind1 <- ind[!is.na(x1) & is.na(x2)]
	ind2 <- ind[is.na(x1) & !is.na(x2)]
	if(length(ind1) > 0)
		lx1 <- ind1 + 1
	if(length(ind2) > 0)
		lx2 <- ind2
	if(length(ind1) == 0)
		lx1 <- 1
	if(length(ind2) == 0)
		lx2 <- n
	if(!is.na(x[1]))
		lx1 <- unique(c(1, lx1))
	if(!is.na(x[n]))
		lx2 <- unique(c(lx2, n))
	nlx <- length(lx1)
	return(list(lx1 = lx1, lx2 = lx2, nlx = nlx))
}
geopolygon<-
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
findcut<-
function(x, xb, in.or.out)
{
	if(!is.data.frame(x))
		x <- data.frame(x = x$x, y = x$y)
	xr <- yr <- mark <- side <- s <- t <- rep(0, (length(x$y) + length(
		xb$y)))
	nxr <- 0
	ab <- ab1 <- rep(0, length(xb$x))
	xr <- .C("define_poly",
		as.double(x$x),
		as.double(x$y),
		as.double(xb$x),
		as.double(xb$y),
		as.double(xr),
		as.double(yr),
		as.integer(length(x$y)),
		as.integer(length(xb$y)),
		as.integer(nxr),
		as.integer(mark),
		as.integer(side),
		as.double(s),
		as.double(t),
		as.double(ab),
		as.double(ab1),
		as.integer(in.or.out))
	nxr <- xr[[9]]
	yr <- xr[[6]][1:nxr]
	mark <- xr[[10]][1:nxr]
	side <- xr[[11]][1:nxr]
	s <- xr[[12]][1:nxr]
	t <- xr[[12]][1:nxr]
	xr <- xr[[5]][1:nxr]
	ind <- c(1:nxr)
	ind2 <- ind[mark == 2]
	i <- geoinside(x[1,  ], reg = xb, option = 3, col.names = c("x", "y"))
	if(in.or.out == 1)
		i <- !i
	if(i == 1 && length(ind2) == 0)
		return(list(x = x$x, y = x$y))
	if(i == 0 && length(ind2) == 0)
		return(invisible())
	if(ind2[1] == 1)
		ind1 <- c(1:nxr)
	else ind1 <- c(ind2[1]:nxr, 1:(ind2[1] - 1))
	xr <- xr[ind1]
	yr <- yr[ind1]
	mark <- mark[ind1]
	side <- side[ind1]
	s <- s[ind1]
	t <- t[ind1]
	h1 <- side + s + 1
	inn <- ifelse(mark == 2, 1, 0)
	ind1 <- ind[mark == 1 | mark == 2]
	nr <- ind[mark == 1 | mark == 2]
	h <- h1[ind1]
	n <- length(h)
	if(n < 2)
		return(invisible())
	# vidbot i profun
	s <- matrix(0, n, 3)
	s[, 2] <- match(sort(h), h)
	s[, 1] <- c(s[n, 2], s[1:(n - 1), 2])
	s[, 3] <- c(s[2:n, 2], s[1, 2])
	o <- match(h, sort(h))
	s <- s[o,  ]
	up <- rep(0, nrow(s))
	pt <- h[s[, 2]] - 0.0001
	i <- geoinside(find.hnit(pt, xb), reg = x, option = 0, col.names = c(
		"x", "y"))
	if(in.or.out == 1) {
		i1 <- c(1:nrow(find.hnit(pt, xb)))
		i <- i1[is.na(match(i1, i))]
	}
	if(length(i) > 0) {
		s[ - i, 1] <- s[ - i, 3]
		up[ - i] <- 1
	}
	s <- matrix(c(s[, 2], s[, 1]),  , 2)
	s1 <- matrix(0, length(h1), 2)
	s1[ind1,  ] <- s
	up1 <- buid <- rep(0, length(h1))
	up1[ind1] <- up
	s1[, 2] <- match(s1[, 2], s1[, 1])
	s1[, 1] <- 1:nrow(s1)
	nxr <- 0
	xr1 <- yr1 <- rep(0, (length(x$x) + length(xb$x)))
	x <- .C("post_filter",
		as.integer(s1[, 2]),
		as.integer(side),
		as.integer(up1),
		as.integer(mark),
		as.double(xr),
		as.double(yr),
		as.integer(buid),
		as.integer(nrow(s1)),
		as.double(xb$x),
		as.double(xb$y),
		as.integer(length(xb$y)),
		as.double(xr1),
		as.double(yr1),
		as.integer(nxr))
	nxr <- x[[14]]
	xr <- x[[12]][1:nxr]
	yr <- x[[13]][1:nxr]
	ind <- c(1:nxr)
	ind <- ind[xr < -999998]
	if(length(ind) > 0)
		xr[ind] <- yr[ind] <- NA
	return(list(x = xr, y = yr, nxr = nxr))
}
cut.multipoly<-
function(x, xb, in.or.out = 0)
{
	ind <- x$x[is.na(x$x)]
	if(length(ind) == 0) {
		x2 <- findcut(x, xb, in.or.out)
	}
	else {
		x2 <- list(x = NA, y = NA)
		ind <- prepare.line(x$x)
		for(i in 1:ind$nlx) {
			x1 <- list(x = x$x[ind$lx1[i]:ind$lx2[i]], y = x$y[
				ind$lx1[i]:ind$lx2[i]])
			x1 <- findcut(x1, xb, in.or.out)
			x2$x <- c(x2$x, NA, x1$x)
			x2$y <- c(x2$y, NA, x1$y)
		}
		x2$x <- x2$x[ - c(1:2)]
		x2$y <- x2$y[ - c(1:2)]
	}
	return(x2)
}
 find.hnit<-
function(pt, poly)
{
	pt1 <- floor(pt)
	pt2 <- pt - pt1
	y <- poly$y[pt1] + pt2 * (poly$y[pt1 + 1] - poly$y[pt1])
	x <- poly$x[pt1] + pt2 * (poly$x[pt1 + 1] - poly$x[pt1])
	return(data.frame(x = x, y = y))
}

 geoaxis <- 
function(side, pos, dist, dlat = 0.5, dlon = 1,csi=0.12, cex = 0.7, inside = T, r = 1,
	...)
{
	m <- par()$cex * csi
	if(inside)
		m <-  - m
	if(side == 2 || side == 4)
		ratio <- diff(geopar$origin$lon)/geopar$gpar$pin[1]
	else ratio <- diff(geopar$origin$lat)/geopar$gpar$pin[2]
	if(side == 2 || side == 4) {
		if(missing(pos)) {
			pos1 <- (geopar$origin$lat[1] %/% dlat) * dlat - dlat
			pos2 <- (geopar$origin$lat[2] %/% dlat) * dlat + dlat
			pos <- seq(pos1, pos2, by = dlat)
		}
		pos <- pos[pos <= geopar$origin$lat[2] & pos >= geopar$origin$
			lat[1]]
		if(missing(dist)) {
			if((side == 4 && inside) || (side == 2 && !inside)) {
				lat1 <- pos %% 1
				lat2 <- lat1 * 60 %% 1
				if(any(lat2))
					lm <- 8
				else if(any(lat1))
					lm <- 6
				else lm <- 4
				dist <- lm * m * 0.6 * r
			}
			else if((side == 2 && inside) || (side == 4 && !inside)
				)
				dist <- m/2.5 * r
		}
	}
	else if(side == 3 || side == 1) {
		if(missing(dist))
			dist <- m * r
		if(missing(pos)) {
			pos1 <- (geopar$origin$lon[1] %/% dlon) * dlon - dlon
			pos2 <- (geopar$origin$lon[2] %/% dlon) * dlon + dlon
			pos <- seq(pos1, pos2, by = dlon)
		}
		pos <- pos[pos <= geopar$origin$lon[2] & pos >= geopar$origin$
			lon[1]]
	}
	if(side == 2 || side == 4) {
		if(side == 2)
			lonpos <- geopar$origin$lon[1] - ratio * dist
		if(side == 4)
			lonpos <- geopar$origin$lon[2] + ratio * dist
		lat1 <- trunc(pos)
		lat2 <- pos %% 1
		txt <- paste(lat1, "", sep = "")
		lat2 <- round(lat2 * 60, 2)
		i <- lat2 > 0
		if(any(i))
			txt[i] <- paste(txt[i], lat2[i], "'", sep = "")
		geotext(pos, rep(lonpos, length(lat1)), adj = 0, txt, cex = cex,
			outside = T, ...)
	}
	else {
		if(side == 1)
			latpos <- geopar$origin$lat[1] - ratio * dist
		if(side == 3)
			latpos <- geopar$origin$lat[2] + ratio * dist
		pos1 <- abs(pos)
		lon1 <- trunc(pos1)
		lon2 <- pos1 %% 1
		txt <- paste(lon1, "", sep = "")
		lon2 <- round(lon2 * 60, 2)
		i <- lon2 > 0
		if(any(i))
			txt[i] <- paste(txt[i], lon2[i], "'", sep = "")
		geotext(rep(latpos, length(pos)), pos, txt, adj = 0.5, cex = 
			cex, outside = T, ...)
	}
}


 geotext <- 
function(lat, lon = 0, z, cex = 0.7, adj = 0.5, col = 1, digits = 0, pretext
	 = "", lwd = 0, aftertext = "", outside = F, angle = 0, jitter = NULL,csi=NULL)
{
    if(!is.null(csi)) cex <- cex*csi/0.12 # For compatibility
	if(length(lon) == 1 && length(lat) > 1) {
		if(geopar$projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	if(geopar$projection != "none") {
		# degrees and minutes
		if(mean(lat, na.rm = T) > 1000) {
			lat <- geoconvert(lat)
			lon <-  - geoconvert(lon)
		}
	}
	if(!is.null(jitter)) {
		lat <- lat + runif(length(lat), -1, 1) * jitter
		lon <- lon + runif(length(lon), -1, 1) * jitter * 2
	}
        
	oldpar <- selectedpar()
	par(geopar$gpar)
        if(outside)
		par(xpd = T)
	else par(xpd = F)
	if(lwd != 0)
		par(lwd = lwd)
	on.exit(par(oldpar))
	par(cex = cex)
	par(adj = adj)
	xx <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection)
	ind <- c(1:length(xx$x))
	if(is.character(z)) {
		if(pretext == "")
			txt <- z
		else txt <- paste(pretext, z, sep = "")
		if(aftertext != "")
			txt <- paste(txt, aftertext, sep = "")
	}
	else {
		if(pretext == "")
			txt <- format(round(z, digits = digits))
		else txt <- paste(pretext, format(round(z, digits = digits)),
				sep = "")
		if(aftertext != "")
			txt <- paste(txt, aftertext, sep = "")
	}
	if(!outside) {
		ind <- c(1:length(xx$x))
		ind <- ind[(!is.na(xx$x)) & (xx$x < geopar$limx[1] | xx$x >
			geopar$limx[2] | xx$y < geopar$limy[1] | xx$y > geopar$
			limy[2])]
		xx$x[ind] <- NA
		xx$y[ind] <- NA
	}
	if(length(angle) == length(xx$x) || length(col) == length(xx$x)) {
		if(length(angle) < length(xx$x))
			angle <- rep(angle[1], length(xx$x))
		if(length(col) < length(xx$x))
			col <- rep(col[1], length(xx$x))
		for(i in 1:length(xx$x)) {
			text(xx$x[i], xx$y[i], txt, col = col[i], srt = angle[
				i])
		}
	}
	else text(xx$x, xx$y, txt, col = col, srt = angle)
	return(invisible())
}
geoconvert<-
function(data, inverse = F, col.names = c("lat", "lon"))
{
	if(!inverse) {
		if(is.data.frame(data)) {
			if(any(is.na(match(col.names, names(data))))) {
				cat(paste("Columns", colnames, "do not exist"))
				return(invisible())
			}
			data[, col.names[1]] <- geoconvert.1(data[, col.names[
				1]])
			data[, col.names[2]] <- geoconvert.1(data[, col.names[
				2]])
		}
		else data <- geoconvert.1(data)
	}
	else {
		# Convert to write out. 
		if(is.data.frame(data)) {
			if(any(is.na(match(col.names, names(data))))) {
				cat(paste("Columns", colnames, "do not exist"))
				return(invisible())
			}
			data[, col.names[1]] <- geoconvert.2(data[, col.names[
				1]])
			data[, col.names[2]] <- geoconvert.2(data[, col.names[
				2]])
		}
		else data <- geoconvert.2(data)
	}
	return(data)
}
geoconvert.1<-
function(x)
{
	i <- sign(x)
	x <- abs(x)
	# x <- ifelse(abs(x) < 10000, x * 100, x) # This can not be allowed.  
	# Check for minutes > 60
	x1 <- x %% 10000
	k <- c(1:length(x1))
	k <- k[x1 > 5999 & !is.na(x1)]
	if(length(k) > 0)
		print(paste("error > 60 min nr", k, x[k]))
	min <- (x/100) - trunc(x/10000) * 100
	return((i * (x + (200/3) * min))/10000)
}
geoconvert.2<-
function(lat)
{
	i <- sign(lat)
	lat <- abs(lat)
	p1 <- floor(lat)
	p2 <- floor((lat - p1) * 60)
	p3 <- round((lat - p1 - p2/60) * 100 * 60)
	return(i * (p1 * 10000 + p2 * 100 + p3))
}
geoinside<-
function(data, reg, option = 1, col.names = c("lat", "lon"), na.rm = T, robust
	 = F)
{
	if(!is.data.frame(data)) {
		i <- match(col.names, names(data))
		data <- data.frame(data[[i[1]]], data[[i[2]]])
		names(data) <- col.names
	}
	i <- match(col.names, names(data))
	index <- rep(NA, nrow(data))
	j <- rep(T, nrow(data))
	tmp <- data
	if(na.rm) {
		j <- !is.na(data[, i[1]]) & !is.na(data[, i[2]])
		data <- data[j,  ]
	}
	i1 <- match(col.names, names(reg))
	regx <- reg[[i1[1]]]
	regy <- reg[[i1[2]]]
	n <- length(regx)
	k <- (regx[1] != regx[n] || regy[1] != regy[n]) && length(regx) != 2
	if(k && !is.na(k)) {
		regx <- c(regx, regx[1])
		regy <- c(regy, regy[1])
	}
	reg <- list(x = regx, y = regy)
	if(length(reg$x) == 2)
		reg <- list(x = c(reg$x[1], reg$x[2], reg$x[2], reg$x[1], reg$
			x[1]), y = c(reg$y[1], reg$y[1], reg$y[2], reg$y[2],
			reg$y[1]))
	data <- list(x = data[[i[1]]], y = data[[i[2]]])
	border <- adapt(reg$y, reg$x, projection = "none")
	inside <- rep(0, length(data$x))
	# Robust method using trigonometric functions.  
	if(robust) {
		a <- a1 <- rep(0, length(reg$x))
		inside <- .C("marghc",
			as.double(data$x),
			as.double(data$y),
			as.integer(length(data$y)),
			as.double(border$x),
			as.double(border$y),
			as.integer(length(border$y)),
			as.integer(border$lxv),
			as.integer(length(border$lxv)),
			as.integer(inside),
			as.double(a),
			as.double(a1))
		inside <- inside[[9]]
	}
	else {
		# Faster method.  
		tmpinside <- rep(0, length(border$lxv))
		inside <- .C("geomarghc",
			as.double(data$x),
			as.double(data$y),
			as.integer(length(data$y)),
			as.double(border$x),
			as.double(border$y),
			as.integer(border$lxv),
			as.integer(length(border$lxv)),
			as.integer(inside),
			as.integer(tmpinside))
		inside <- inside[[8]]
	}
	index[j] <- inside
	inside <- index
	ind <- c(1:length(inside))
	ind <- ind[inside > 0 & !is.na(inside)]
	if(option == 1) {
		tmp <- tmp[ind,  ]
		return(tmp)
	}
	else if(option == 2) {
		tmp <- tmp[ - ind,  ]
		return(tmp)
	}
	else if(option == 3)
		return(inside)
	else if(option == 4)
		return(1 - inside)
	else if(option == 5) {
		ind <- c(1:length(inside))
		ind <- ind[inside == 0]
		return(ind)
	}
	else if(option == 6) {
		ind <- c(1:length(inside))
		ind <- ind[inside != 0]
		return(ind)
	}
	else return(ind)
}
adapt<-
function(reg.lat, reg.lon, projection = "Mercator")
{
	ind <- c(1:length(reg.lat))
	nholes <- length(reg.lat[is.na(reg.lat)])
	lxv <- c(0:(nholes + 1))
	if(nholes != 0) {
		#               remove NA,s and points given twice
		ind1 <- ind[is.na(reg.lat)]
		ind2 <- c(ind1 - 1, ind1, length(reg.lat))
		lon <- reg.lon[ - ind2]
		lat <- reg.lat[ - ind2]
		for(i in 2:(nholes + 1)) {
			lxv[i] <- ind1[i - 1] - 2 * (i - 1)
		}
	}
	else {
		ind <- (1:length(reg.lon) - 1)
		lon <- reg.lon[ind]
		lat <- reg.lat[ind]
	}
	lxv[nholes + 2] <- length(lon)
	#x,y coordinates.   
	if(projection == "none") return(list(x = lon, y = lat, lxv = lxv))
		 else return(list(lat = lat, lon = lon, lxv = lxv))
}
geosubplot<-
function(fun, pos, size = c(2, 2), fill, fillcol, ...)
{
	if(length(pos$lat) == 1) {
		# Calculate new limits.
		plt.size <- par()$pin
		rlon <- (diff(geopar$origin$lon) * size[1])/plt.size[1]
		rlat <- (diff(geopar$origin$lat) * size[2])/plt.size[2]
		pos <- data.frame(lat = pos$lat + c(-0.5, 0.5) * rlat, lon = 
			pos$lon + c(-0.5, 0.5) * rlon)
	}
	if(!missing(fill)) {
		if(!missing(fillcol))
			geopolygon(pos, col = fillcol)
		else geopolygon(pos, col = 0)
	}
	pos <- Proj(pos)
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	pr <- subplot(fun, pos, ...)
	return(invisible())
}

subplot <-
function(fun, x, y, size = c(1, 1), vadj = 0.5, hadj = 0.5, pars)
{
	if(missing(fun))
		stop("missing argument \"fun\"")
	if(missing(pars)) {
		# set graphical parameters
		opars <- selectedpar()#(no.readonly = TRUE)
		# save old parameters
		par(err = -1)
		fin <- par()$fin
		# dimensions of figure, inches
		#
		#     mai doesn't deal with pty='s', etc
		#     mai <- par()$mai	# bottom, left, top, right margins, in inches
		pltin <- par("plt") * fin[c(1, 1, 2, 2)]
		mai <- c(pltin[3], pltin[1], fin[2] - pltin[4], fin[1] - pltin[
			2])
		#
		#
		usr <- par()$usr
		# limits in user units : xmin,xmax,ymin,ymax
                # uin paramter does not exist in R.  
		uin <- par()$pin/(c(usr[2]-usr[1],usr[4]-usr[3]))#par()$uin
		# inches per user units , x then y.
		if(missing(x)) if(missing(size)) {
				cat("Using function \"locator(2)\" to place opposite corners of subplot\n"
					)
				x <- locator(2)
			}
			else {
				cat("Using function \"locator(1)\" to place subplot\n"
					)
				x <- locator(1)
			}
		if(!is.null(x$x) && !is.null(x$y)) {
			y <- x$y
			x <- x$x
		}
		if(length(x) == 2 && length(y) == 2) {
			# then x,y represent corners of plot
			# reparameterize to lower left corner, size
			x <- sort(x)
			y <- sort(y)
			size[1] <- (x[2] - x[1]) * uin[1]
			size[2] <- (y[2] - y[1]) * uin[2]
			x <- x[1]
			y <- y[1]
			hadj <- 0
			vadj <- 0
		}
		if(length(x) != 1 || length(y) != 1)
			stop("length of x and y must both be same: 1 or 2")
		# convert x, y to inches from edges of plot, xi and yi
		xi <- mai[2] + (x - usr[1]) * uin[1]
		yi <- mai[1] + (y - usr[3]) * uin[2]
		hoff <- size[1] * hadj
		voff <- size[2] * vadj
		newmai <- c(yi - voff, xi - hoff, fin[2] - yi - size[2] + voff,
			fin[1] - xi - size[1] + hoff)
		newmex <- sqrt(max(size)/min(fin))
		if(any(newmai < 0))
			stop("subplot out of bounds")
		par(pty = "m", mex = newmex, mai = newmai)
	}
	else opars <- par(pars)
	# don't set new graphical parameters
	opars$new <- F
	#sometimes axes-less image plots will make it stick
	par(new = T)
	# don't erase current plot
	on.exit(par(opars))
	# don't erase current plot
	eval(fun, sys.parent(1))
	invisible(par()[names(opars)])
}

geopoints<-
function(lat, lon = 0, pch = "*",cex =0.7, col = 1, lwd = 0, outside = F,
	jitter = NULL, mkh = NULL,csi=NULL)
{
   if(!is.null(csi)) cex <- cex*csi/0.12  # Compatibility with old program
	if(length(lon) == 1 && length(lat) > 1) {
		if(geopar$projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	if(geopar$projection != "none") {
		# degrees and minutes
		if(mean(lat, na.rm = T) > 1000) {
			lat <- geoconvert(lat)
			lon <-  - geoconvert(lon)
		}
	}
	if(!is.null(jitter)) {
		lat <- lat + runif(length(lat), -1, 1) * jitter
		lon <- lon + runif(length(lon), -1, 1) * jitter * 2
	}
	oldpar <- selectedpar()
	par(geopar$gpar)
	if(lwd != 0)
		par(lwd = lwd)
	if(outside)
		par(xpd = T)
	else par(xpd = F)
	on.exit(par(oldpar))
	par(cex = cex)
	xx <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection)
	if(!outside) {
		ind <- c(1:length(lat))
		ind <- ind[xx$x > geopar$limx[2] | xx$x < geopar$limx[1] | xx$
			y < geopar$limy[1] | xx$y > geopar$limy[2] | is.na(
			xx$x) | is.na(xx$y)]
		if(length(ind) > 0) {
			xx$x <- xx$x[ - ind]
			xx$y <- xx$y[ - ind]
		}
	}
	if(!is.null(mkh))
		points(xx$x, xx$y, pch = pch, col = col, mkh = mkh)
	else points(xx$x, xx$y, pch = pch, col = col)
	return(invisible())
}
geozoom<-
function()
{
	if(as.character(geopar$command[length(geopar$command)]) != "123")
		com <- c(geopar$command, zoom = 123)
	else com <- geopar$command
	eval(com)
}
gbplot<-
function(depth, col, lty, lwd, depthlab, depthlabcex)
{
	if(missing(depthlabcex))
		depthlabcex <- 0.7
	if(missing(lwd))
		lwd <- rep(1, length(depth))
	if(missing(lty))
		lty <- rep(1, length(depth))
	if(missing(col))
		col <- rep(1, length(depth))
	if(length(col) < length(depth))
		col[(length(col) + 1):length(depth)] <- col[length(col)]
	if(length(lwd) < length(depth))
		lwd[(length(lwd) + 1):length(depth)] <- lwd[length(lwd)]
	if(length(lty) < length(depth))
		lty[(length(lty) + 1):length(depth)] <- lty[length(lty)]
	for(i in 1:length(depth)) {
		dypi <- depth[i]
		if(dypi %% 100 != 0 || dypi == 300 || dypi == 700) {
			print(paste(dypi, "m line does not exist in GEBCO data"))
			return(invisible())
		}
		if(dypi <= 1000 || dypi == 1200 || dypi == 1500 || dypi == 2000
			)
			txt <- paste("geolines(gbdypi.", dypi, 
				",col=col[i],lwd=lwd[i],lty=lty[i])", sep = "")
		else {
			j <- match(dypi, names(gbdypi))
			txt <- paste("geolines(gbdypi[[", j, 
				"]],col=col[i],lwd=lwd[i],lty=lty[i])", sep = 
				"")
		}
		eval(parse(text = txt))
		if(!missing(depthlab)) {
			k <- !is.na(match(depthloc$z, dypi))
			if(any(k))
				geotext(depthloc[k,  ], z = depthloc[k, "z"],
					cex = depthlabcex)
		}
	}
	return(invisible())
}

geosymbols<-
function(lat, lon = 0, z, levels = NULL, reflevels = NULL, labels.only = F,
	cex = 0.6, chs = 0.8, z1 = 0, circles = 0, squares = 0, rectangles = c(
	0, 0), vbars = 0, hbars = 0, perbars = 0, parbars = 0, sqrt = F, col = 
	1, maxn = 0, colplot = F, nlevels = 10, colors = 0, n = 25, maxcol = 
	155, only.positive = F, digits = 0, white = F, lwd = 1, label.location
	 = NULL, labels = 1, fill.circles = F, density = 0, angle = 45, rotate
	 = 0, outside = F, minsym = "<", boundcheck = 0, na.rm = T, 
	label.resolution = 0, characters = F, pch, marks, charcol = 0, 
	open.circles = F, col.names = c("lat", "lon"), border = F, bordercol = 
	0)
{
	options(warn = -1)
	if(!is.null(label.location))
		if(is.list(label.location))
			label.location <- as.data.frame(label.location)
	if(is.data.frame(lat)) {
		i <- match(col.names, names(lat))
		data <- data.frame(lat = lat[, i[1]], lon = lat[, i[2]])
	}
	else {
		data <- data.frame(lat = lat, lon = lon)
	}
	if(na.rm) {
		# delete na.
		ind <- c(1:length(data$lat))
		ind <- ind[is.na(data$lat) | is.na(data$lon)]
		if(length(ind) > 0) {
			data$lat <- data$lat[ - ind]
			data$lon <- data$lon[ - ind]
			z <- z[ - ind]
		}
	}
	ind <- c(1:length(data$lat))
	ind <- ind[is.na(z)]
	if(length(ind) > 0) {
		data$lat[ind] <- NA
		data$lon[ind] <- NA
		z[ind] <- mean(z, na.rm = T)
	}
	if(fill.circles)
		colplot <- T
	if(open.circles)
		colplot <- T
	if(density > 0)
		colplot <- T
	if(maxn == 0)
		maxn <- max(abs(z))
	if(only.positive) {
		ind <- c(1:length(z))
		ind <- ind[z < 0]
		z[ind] <- 0
	}
	if(boundcheck != 0) {
		dataprj <- Proj(data$lat, data$lon)
		ind <- c(1:length(data$lat))
		ind <- ind[dataprj$x < geopar$limx[1] | dataprj$x > geopar$
			limx[2] | dataprj$y < geopar$limy[1] | dataprj$y > 
			geopar$limy[2]]
		if(length(ind) > 0) {
			ind1 <- paste(ind, collapse = ",")
			print(paste("points", ind1, "out of bounds"))
		}
		if(boundcheck == 2) {
			if(length(ind) > 0) {
				data$lat <- data$lat[ - ind]
				data$lon <- data$lon[ - ind]
				z <- z[ - ind]
			}
		}
	}
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	if(outside)
		par(xpd = T)
	else par(xpd = F)
	if(colplot) {
		if(labels.only) {
			if(cex != 0)
				par(cex = cex)
			colsymbol(data$lat, data$lon, z, circles, squares,
				rectangles, hbars, vbars, perbars, parbars,
				levels, nlevels, colors, white, n, maxcol,
				digits, label.location, labels, fill.circles,
				density, angle, rotate, minsym, 
				label.resolution, col, labels.only = T, 
				open.circles = open.circles, lwd = lwd, border
				 = border, bordercol = bordercol)
		}
		else {
			if(cex != 0)
				par(cex = cex)
			colsymbol(data$lat, data$lon, z, circles, squares,
				rectangles, hbars, vbars, perbars, parbars,
				levels, nlevels, colors, white, n, maxcol,
				digits, label.location, labels, fill.circles,
				density, angle, rotate, minsym, 
				label.resolution, col, open.circles = 
				open.circles, lwd = lwd, border = border, 
				bordercol = bordercol)
		}
	}
	else {
		x <- Proj(data$lat, data$lon, geopar$scale, geopar$b0, geopar$
			b1, geopar$l1, geopar$projection)
		y <- x$y
		x <- x$x
		ein.pr.in <- (geopar$limy[2] - geopar$limy[1])/geopar$gpar$
			pin[2]
		if(!is.null(label.location)) {
			if(label.location == "locator" || label.location == 0)
				label.location <- geolocator(n = 2)
			limits <- Proj(label.location)
			xlim <- limits$x
			ylim <- limits$y
			if(xlim[1] > xlim[2]) {
				temp <- xlim[1]
				xlim[1] <- xlim[2]
				xlim[2] <- temp
			}
			if(ylim[1] > ylim[2]) {
				temp <- ylim[1]
				ylim[1] <- ylim[2]
				ylim[2] <- temp
			}
			if(is.null(levels)) {
				rg <- range(z)
				lrg <- rg[2] - rg[1]
				levels <- signif(seq(rg[1] + lrg/10, rg[2] -
					lrg/10, length = nlevels), 2)
			}
			lbox <- length(levels)
			boxy <- c(1:lbox)
			boxy <-  - boxy/lbox + 1
			boxy1 <- boxy + 1/(1.2 * lbox)
			yloc <- (boxy + boxy1)/2
			xloc <- matrix(0.85, length(yloc))
			par(adj = 0)
			textx <- as.character(levels)
			boxx <- c(matrix(0.1, 1, length(boxy)))
			boxx <- xlim[1] + abs((xlim[2] - xlim[1])) * boxx
			xloc <- xlim[1] + abs((xlim[2] - xlim[1])) * xloc
			yloc <- ylim[1] + abs((ylim[2] - ylim[1])) * yloc
			boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
			ll <- (ylim[2] - ylim[1]) * 0.05
			if(circles != 0 | squares != 0 | hbars != 0 | vbars !=
				0 | perbars != 0)
				text(boxx, boxy + ll, textx, cex = chs)
		}
		if(circles != 0) {
			# plot circles.
			rg <- range(circles)
			rglen <- rg[2] - rg[1]
			lev <- seq(rg[1] + rglen/10, rg[2] - rglen/10, length
				 = 5)
			if((circles > 100) | (circles < 0))
				circles <- 0.2
			#default value.  
			circles <- ein.pr.in * circles
			# size in units
			if(sqrt) {
				if(!labels.only)
					symbols(x, y, circles = circles * sqrt(
						abs(z)/maxn), inches = F, add
						 = T, fg = col, lwd = lwd)
				if(!is.null(label.location))
					symbols(c(xloc), c(yloc), circles = 
						circles * sqrt(abs(levels)/
						maxn), add = T, inches = F,
						lwd = lwd,fg=col)
			}
			else {
				if(!labels.only)
					symbols(x, y, circles = circles * (
						abs(z)/maxn), add = T, inches
						 = F, fg = col, lwd = lwd)
				if(!is.null(label.location))
					symbols(c(xloc), c(yloc), circles = (
						circles * abs(levels))/maxn,
						add = T, inches = F, lwd = lwd,fg=col)
			}
		}
		if(squares != 0) {
			#plot squares.
			if((squares > 100) | (squares < 0)) squares <- 0.2
			#default value.  
			squares <- ein.pr.in * squares
			# size in units  
			if(sqrt) {
				if(!labels.only)
					symbols(x, y, squares = squares * sqrt(
						abs(z)/maxn), add = T, inches
						 = F, fg = col, lwd = lwd)
				symbols(c(xloc), c(yloc), squares = squares *
					sqrt(abs(levels)/maxn), add = T, inches
					 = F, lwd = lwd,fg=col)
			}
			else {
				if(!labels.only)
					symbols(x, y, squares = squares * (
						abs(z)/maxn), add = T, inches
						 = F, fg = col, lwd = lwd)
				symbols(c(xloc), c(yloc), squares = (squares *
					abs(levels))/maxn, add = T, inches = F,fg=col,
					lwd = lwd)
			}
		}
		if((rectangles[1] != 0) | (rectangles[2] != 0)) {
			# plot rectangles
			if((rectangles[1] > 100) | (rectangles[1] < 0)) 
					rectangles[1] <- 0.2
			# thickness
			if((rectangles[2] > 100) | (rectangles[2] < 0)) 
					rectangles[2] <- 0.2
			# length
			rectangles[1] <- ein.pr.in * rectangles[1]
			# size in units  
			rectangles[2] <- ein.pr.in * rectangles[2]
			# size in units  
			m <- matrix(rectangles[1], length(z), 2)
			if(sqrt)
				m[, 1] <- rectangles[1] * sqrt(abs(z)/maxn)
			else m[, 1] <- (rectangles[1] * abs(z))/maxn
			if(length(z1) > 1) {
				if(sqrt)
					m[, 2] <- rectangles[2] * sqrt(abs(
						z1)/max(abs(z1)))
				else m[, 2] <- (rectangles[2] * abs(z1))/max(
						abs(z1))
			}
			symbols(x, y, rectangles = m, add = T, inches = F,
				fg = col, lwd = lwd)
		}
		if(vbars != 0) {
			# plot vertical bars
			if(vbars > 100) vbars <- 0.4
			mx <- matrix(NA, 3, length(x))
			my <- mx
			mx[1,  ] <- x
			my[1,  ] <- y
			mx[2,  ] <- x
			#      mlocx<- matrix(NA,3,length(levels)); mlocy<-mlocx
			#      mlocx[1,]<-c(xloc) ; mlocy[1,]<-c(yloc); mlocx[2,]<-c(xloc)
			r <- ein.pr.in * vbars
			# size in units  
			if(sqrt) {
				my[2,  ] <- my[1,  ] + r * sqrt(abs(z)/maxn)
			}
			else {
				my[2,  ] <- my[1,  ] + (r * abs(z))/maxn
			}
			if(!labels.only)
				lines(mx, my, col = col, lwd = lwd)
		}
		if(hbars != 0) {
			# plot horizontal bars
			if(hbars > 100) hbars <- 0.4
			mx <- matrix(NA, 3, length(x))
			my <- mx
			mx[1,  ] <- x
			my[1,  ] <- y
			my[2,  ] <- y
			#      mlocx<- matrix(NA,3,length(levels)); mlocy<-mlocx
			#      mlocx[1,]<-c(xloc) ; mlocy[1,]<-c(yloc); mlocy[2,]<-c(yloc)
			r <- ein.pr.in * hbars
			# size in units
			if(sqrt) {
				mx[2,  ] <- mx[1,  ] + r * sqrt(abs(z)/maxn)
			}
			else {
				mx[2,  ] <- mx[1,  ] + (r * abs(z))/maxn
			}
			if(!labels.only)
				lines(mx, my, col = col, lwd = lwd)
		}
		if(perbars != 0) {
			# plot bars perpendicular to cruiselines
			if(perbars > 100) perbars <- 0.4
			mx <- matrix(NA, 3, length(x))
			my <- mx
			mx[1,  ] <- x
			my[1,  ] <- y
			#      mlocx<- matrix(NA,3,length(levels)); mlocy<-mlocx
			#      mlocx[1,]<-c(xloc) ; mlocy[1,]<-c(yloc); mlocy[2,]<-c(yloc)
			r <- ein.pr.in * perbars
			# size in units  
			dx <- c(1:length(x))
			dx[1] <- x[2] - x[1]
			dx[2:(length(x) - 1)] <- x[3:(length(x))] - x[1:(length(
				x) - 2)]
			dx[length(x)] <- x[length(x)] - x[length(x) - 1]
			dy <- c(1:length(y))
			dy[1] <- y[2] - y[1]
			dy[2:(length(y) - 1)] <- y[3:length(y)] - y[1:(length(
				y) - 2)]
			dy[length(y)] <- y[length(x)] - y[length(y) - 1]
			dxy <- sqrt(dx * dx + dy * dy)
			dx <- dx/dxy
			dy <- dy/dxy
			if(sqrt)
				mx[2,  ] <- mx[1,  ] - dy * r * sqrt(abs(z)/
					maxn)
			else mx[2,  ] <- mx[1,  ] - (dy * r * abs(z))/maxn
			if(sqrt)
				my[2,  ] <- my[1,  ] + dx * r * sqrt(abs(z)/
					maxn)
			else my[2,  ] <- my[1,  ] + (dx * r * abs(z))/maxn
			#      if(sqrt)  mlocx[2,]<-mlocx[1,]+r*sqrt(abs(levels)/maxn)
			#      else  mlocx[2,]<-mlocx[1,]+r*abs(z)/maxn
			if(!labels.only) lines(mx, my, col = col)
		}
	}
	par(oldpar)
	if(characters) {
		if(missing(marks))
			marks <- rep(-1, length(pch))
		if(missing(pch))
			pch <- rep(" ", length(marks))
		if(!is.numeric(levels)) {
			if(!is.numeric(z))
				ind <- match(z, levels)
			else {
				if(is.null(reflevels)) {
					print("Error")
					return()
				}
				ind <- match(z, reflevels)
			}
			n <- length(levels)
		}
		else {
			if(charcol != 0)
				col <- charcol
			n <- length(levels) + 1
			levels <- c(-1000000., levels, 1000000.)
			ind <- cut(z, levels,labels=FALSE) # labels=FALSE R ver.
		}
		if(length(col) == 1)
			col <- rep(col, n)
		if(length(cex) == 1)
			cex <- rep(cex, n)
		if(!labels.only) {
			for(i in 1:n) {
				tmp <- data[ind == i,  ]
				if(nrow(tmp) > 0) {
					if(marks[i] < 0)
						geopoints(tmp, pch = pch[i],
							cex = cex[i], col = col[
							i])
					else geopoints(tmp, pch = marks[i],
							cex = cex[i], col = col[
							i])
				}
			}
		}
		if(!is.null(label.location)) {
			########
			if(!is.list(label.location)) if(label.location == 
					"locator")
					label.location <- geolocator(n = 2)
			oldpar <- selectedpar()
			on.exit(par(oldpar))
			par(geopar$gpar)
			paint.window(label.location)
			label.location <- Proj(label.location)
			if(is.numeric(levels))
				Pointlabel(levels[2:(length(levels) - 1)],
					digits, label.location$x, 
					label.location$y, minsym, 
					label.resolution, marks, pch, col,
					cex, chs)
			else Charlabel(levels, label.location$x, label.location$
					y, label, marks, pch, col, cex, chs)
		}
	}
	options(warn = 0)
	return(invisible())
}

colsymbol<-
function(lat, lon, z, circles, squares, rectangles, hbars, vbars, perbars,
	parbars, levels, nlevels, colors, white, n, maxcol, digits, 
	label.location, labels, fill.circles, density, angle, rotate, minsym = 
	"<", label.resolution = 0, col = 1, labels.only = F, open.circles,
	lwd, border = F, bordercol = 0)
{
	cont <- levels
	ncont <- nlevels
	z <- z + 1e-07
	# because of zeros.
	if(length(cont) == 1 && cont[1] == -99999) {
		if(ncont == 0)
			ncont <- 10
		cont <- pretty(c(min(z), max(z)), ncont)
		cont <- cont[2:(length(cont) - 1)]
	}
	ncont <- length(cont)
	mcont <- mean( - cont[1:(ncont - 1)] + cont[2:(ncont)])
	cont1 <- cont
	cont <- c(cont, max(z) + mcont * 5)
	cont <- c(min(z) - mcont * 5, cont)
	if(cont[1] >= cont[2])
		cont[1] <- cont[2] - 1
	if(cont[ncont + 2] <= cont[ncont + 1])
		cont[ncont + 2] <- cont[ncont + 1] + 1
	ncont <- ncont + 2
	#	Set colors if needed
	if(length(colors) < 2) {
		if(fill.circles || open.circles) {
			# different size of circles filled 
			colors <- c(1:(ncont - 1))
			if(maxcol > 3)
				maxcol <- 0.1
			colors <- (colors * maxcol)/(ncont - 1)
		}
		else {
			if(density > 0 && maxcol > 70)
				maxcol <- 70
			if(density > 0)
				mincol <- 8
			else mincol <- 2
			if(white) {
				# lowest values white.  
				colors <- c(1:(ncont - 2))
				colors <- floor(mincol + ((colors - 1) * (
					maxcol - mincol))/(length(colors) -
					1))
				colors <- c(0, colors)
			}
			else {
				colors <- c(1:(ncont - 1))
				colors <- floor(mincol + ((colors - 1) * (
					maxcol - mincol))/(length(colors) -
					1))
			}
		}
	}
	#	Define color for each point.  
	ind <- cut(z, cont,labels=FALSE ) # labels=FALSE R ver.
	ind <- colors[ind]
	# number of color.  
	ein.pr.in <- (geopar$limy[2] - geopar$limy[1])/geopar$gpar$pin[2]
	if(fill.circles || open.circles) {
		# different sizes of circles 
		theta <- (c(0:n) * 2 * pi)/n
		theta <- c(theta, NA)
		x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$
			l1, geopar$projection)
		theta <- c(matrix(theta, n + 2, length(z)))
		y <- c(t(matrix(x$y, length(lat), n + 2)))
		x <- c(t(matrix(x$x, length(lon), n + 2)))
		ind1 <- c(t(matrix(ind, length(lon), n + 2)))
		y <- y + ein.pr.in * ind1 * sin(theta)
		x <- x + ein.pr.in * ind1 * cos(theta)
		if(!labels.only) {
			if(fill.circles) {
				polygon(x, y, col = col, border = F)
				if(border)
					lines(x, y, col = bordercol)
			}
			if(open.circles)
				lines(x, y, lwd = lwd, col = col)
		}
	}
	if(circles != 0 && !fill.circles) {
		if((circles > 100) | (circles < 0))
			circles <- 0.05
		#default value.  
		circles <- ein.pr.in * circles
		theta <- (c(0:n) * 2 * pi)/n
		theta <- c(theta, NA)
		theta <- c(matrix(theta, n + 2, length(z)))
		x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$
			l1, geopar$projection)
		if(density > 0) {
			angle1 <- angle
			theta <- (c(0:n) * 2 * pi)/n
			for(i in 1:length(ind)) {
				angle1 <- angle1 + rotate
				y1 <- c(matrix(x$y[i], 1, n + 1))
				x1 <- c(matrix(x$x[i], 1, n + 1))
				x1 <- x1 + circles * cos(theta)
				y1 <- y1 + circles * sin(theta)
				if(!labels.only) {
					polygon(x1, y1, density = ind[i], 
						border = F, angle = angle1,
						col = col)
					if(border && ind[i] == 0)
						lines(x1, y1, col = 1)
				}
			}
		}
		else {
			y <- c(t(matrix(x$y, length(lat), n + 2)))
			x <- c(t(matrix(x$x, length(lon), n + 2)))
			y <- y + circles * sin(theta)
			x <- x + circles * cos(theta)
			if(!labels.only) {
				polygon(x, y, col = ind, border = F)
				if(border)
					lines(x, y, col = 1)
			}
		}
	}
	if(squares != 0 && !fill.circles) {
		if((squares > 100) | (squares < 0))
			squares <- 0.05
		#default value.  
		squares <- ein.pr.in * squares
		theta <- (c(-45, 45, 135, 225) * pi)/180
		theta <- c(theta, NA)
		theta <- c(matrix(theta, 5, length(z)))
		x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$
			l1, geopar$projection)
		y <- c(t(matrix(x$y, length(lat), 5)))
		x <- c(t(matrix(x$x, length(lon), 5)))
		y <- y + squares * sqrt(2) * sin(theta)
		x <- x + squares * sqrt(2) * cos(theta)
		if(!labels.only) {
			polygon(x, y, col = ind, border = F)
			if(border)
				lines(x, y, col = 1)
		}
	}
	if((rectangles[1] != 0 && !fill.circles) | (rectangles[2] != 0)) {
		# plot rectangles
		th <- atan(rectangles[2], rectangles[1])
		th <- c(th, 2 * (pi/2 - th) + th)
		theta <- c(th,  - th)
		theta <- c(theta, NA)
		theta <- c(matrix(theta, 5, length(z)))
		x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$
			l1, geopar$projection)
		y <- c(t(matrix(x$y, length(lat), 5)))
		x <- c(t(matrix(x$x, length(lon), 5)))
		y <- y + squares * sqrt(2) * sin(theta)
		x <- x + squares * sqrt(2) * cos(theta)
		polygon(x, y, col = ind, border = F)
		if(border)
			lines(x, y, col = 1)
	}
	if(vbars != 0 && !fill.circles) {
		# plot vertical bars
		x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$
			l1, geopar$projection)
		y <- x$y
		x <- x$x
		if(vbars > 100)
			vbars <- 0.2
		mx <- matrix(0, 2, length(x))
		my <- mx
		mx[1,  ] <- x
		my[1,  ] <- y
		mx[2,  ] <- x
		my[2,  ] <- my[1,  ] + r
		for(i in 1:ncol(mx))
			lines(mx[, i], my[, i], col = ind[i])
	}
	if(hbars != 0 && !fill.circles) {
		# plot horizontal bars
		x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$
			l1, geopar$projection)
		y <- x$y
		x <- x$x
		if(hbars > 100)
			hbars <- 0.2
		mx <- matrix(0, 2, length(x))
		my <- mx
		mx[1,  ] <- x
		my[1,  ] <- y
		my[2,  ] <- y
		r <- ein.pr.in * hbars
		# size in units  
		mx[2,  ] <- mx[1,  ] + r
		for(i in 1:ncol(mx))
			lines(mx[, i], my[, i], col = ind[i])
	}
	if(perbars != 0 && !fill.circles) {
		# plot bars perpendicular to cruiselines
		x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$
			l1, geopar$projection)
		y <- x$y
		x <- x$x
		if(perbars > 100)
			perbars <- 0.2
		mx <- matrix(0, 2, length(x))
		my <- mx
		mx[1,  ] <- x
		my[1,  ] <- y
		r <- ein.pr.in * perbars
		# size in units  
		dx <- c(1:length(x))
		dx[1] <- x[2] - x[1]
		dx[2:(length(x) - 1)] <- x[3:(length(x))] - x[1:(length(x) -
			2)]
		dx[length(x)] <- x[length(x)] - x[length(x) - 1]
		dy <- c(1:length(y))
		dy[1] <- y[2] - y[1]
		dy[2:(length(y) - 1)] <- y[3:length(y)] - y[1:(length(y) - 2)]
		dy[length(y)] <- y[length(x)] - y[length(y) - 1]
		dxy <- sqrt(dx * dx + dy * dy)
		dx <- dx/dxy
		dy <- dy/dxy
		mx[2,  ] <- mx[1,  ] - dy * r
		my[2,  ] <- my[1,  ] + dx * r
		if(!labels.only)
			for(i in 1:ncol(mx))
				lines(mx[, i], my[, i], col = ind[i])
	}
	if(parbars != 0 && !fill.circles) {
		# colors along transsect lines.  
		x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$
			l1, geopar$projection)
		y <- x$y
		x <- x$x
		nx <- length(x)
		x1 <- x[1:(nx - 1)]
		x2 <- x[2:nx]
		y1 <- y[1:(nx - 1)]
		y2 <- y[2:nx]
		dy1 <- (y2 - y1)
		dx1 <- (x2 - x1)
		x11 <- x1
		y11 <- y1
		r <- ein.pr.in * parbars
		# size in units  
		if(parbars > 100) parbars <- 0.1
		mx <- matrix(NA, 5, length(x1))
		my <- mx
		p1x <- x11 + dx1/2
		p1y <- y11 + dy1/2
		p2x <- x11 - (0 * dx1)/2
		p2y <- y11 - (0 * dy1)/2
		dxy <- sqrt(dx1 * dx1 + dy1 * dy1)
		dx <- dx1/dxy
		dy <- dy1/dxy
		mx[1,  ] <- p1x - (dy * r)/2
		mx[2,  ] <- p1x + (dy * r)/2
		mx[3,  ] <- p2x + (dy * r)/2
		mx[4,  ] <- p2x - (dy * r)/2
		my[1,  ] <- p1y + (dx * r)/2
		my[2,  ] <- p1y - (dx * r)/2
		my[3,  ] <- p2y - (dx * r)/2
		my[4,  ] <- p2y + (dx * r)/2
		if(!labels.only) {
			polygon(mx, my, border = F, col = ind)
			if(border)
				lines(mx, my, col = 1)
		}
		x11 <- x2
		y11 <- y2
		r <- ein.pr.in * parbars
		# size in units  
		if(parbars > 100) parbars <- 0.1
		mx <- matrix(NA, 5, length(x1))
		my <- mx
		p1x <- x11 + (0 * dx1)/2
		p1y <- y11 + (0 * dy1)/2
		p2x <- x11 - dx1/2
		p2y <- y11 - dy1/2
		dxy <- sqrt(dx1 * dx1 + dy1 * dy1)
		dx <- dx1/dxy
		dy <- dy1/dxy
		mx[1,  ] <- p1x - (dy * r)/2
		mx[2,  ] <- p1x + (dy * r)/2
		mx[3,  ] <- p2x + (dy * r)/2
		mx[4,  ] <- p2x - (dy * r)/2
		my[1,  ] <- p1y + (dx * r)/2
		my[2,  ] <- p1y - (dx * r)/2
		my[3,  ] <- p2y - (dx * r)/2
		my[4,  ] <- p2y + (dx * r)/2
		if(!labels.only)
			polygon(mx, my, border = F, col = ind[2:length(ind)])
	}
	# 	Add  labels around plot 
	if(length(label.location) == 1) if(label.location == "locator")
			label.location <- geolocator(n = 2)
	# use the locator.  
	if(length(label.location) > 1) {
		#label located somewhere in drawing
		paint.window(label.location)
		label.location <- Proj(label.location$lat, label.location$
			lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
			geopar$projection)
		if(fill.circles || open.circles) {
			if(fill.circles)
				open <- F
			if(open.circles)
				open <- T
			labels.size(cont1, digits, colors, xlim = 
				label.location$x, ylim = label.location$y,
				n = n, rat = ein.pr.in, minsym = minsym, 
				label.resolution = label.resolution, open = 
				open, lwd = lwd, col = col)
		}
		else if(density > 0)
			shading1(cont1, digits, colors, angle = angle, rotate
				 = rotate, cex = par()$cex, xlim = 
				label.location$x, ylim = label.location$y)
		else {
			if(labels == 1) {
				# labels for each contour line.  
				labels1(cont1, digits, colors, xlim = 
					label.location$x, ylim = label.location$
					y)
			}
			else {
				#more of a constant label. 
				labels2(cont1, digits, colors, xlim = 
					label.location$x, ylim = label.location$
					y)
			}
		}
	}
	if(geopar$cont && labels != 0) {
		# if labels needed.  
		par(plt = geopar$contlab)
		par(new = T)
		plot(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0), type = "l", axes = F,
			xlab = " ", ylab = " ")
		if(density > 0)
			shading1(cont1, digits, colors, angle = angle, rotate
				 = rotate, cex = par()$cex, fill = geopar$
				cont)
		else {
			if(labels == 1) {
				# labels for each contour line.  
				labels1(cont1, digits, colors, fill = geopar$
					cont)
			}
			else {
				#more of a constant label. 
				labels2(cont1, digits, colors, fill = geopar$
					cont)
			}
		}
	}
	return(invisible())
}
labels1<-
function(cont, digits, colors, xlim = c(0, 1), ylim = c(0, 1), fill = F, minsym
	 = "<", label.resolution = 0, labtxt = NULL, first.color.trans = T,
	mai = c(0, 1, 0, 1), leftrat = 0.1)
{
	xlim <- sort(xlim)
	ylim <- sort(ylim)
	dx <- (xlim[2] - xlim[1])
	dy <- (ylim[2] - ylim[1])
	xlim[2] <- xlim[1] + mai[2] * dx
	xlim[1] <- xlim[1] + mai[1] * dx
	ylim[2] <- ylim[1] + mai[4] * dy
	ylim[1] <- ylim[1] + mai[3] * dy
	ncont <- length(cont)
	if(label.resolution == "none")
		lbox <- ncont
	else lbox <- ncont + 1
	if(fill)
		lbox <- max(lbox, 20)
	boxy <- c(1:lbox)
	boxy <-  - boxy/lbox + 1
	boxy1 <- boxy + 1/(1.2 * lbox)
	if(fill) {
		boxy <- boxy[1:(ncont + 1)]
		boxy1 <- boxy1[1:(ncont + 1)]
	}
	ymat <- matrix(0, 5, length(boxy))
	ymat[1,  ] <- boxy
	ymat[2,  ] <- boxy
	ymat[3,  ] <- boxy1
	ymat[4,  ] <- boxy1
	ymat[5,  ] <- NA
	xmat <- matrix(0, 5, length(boxy))
	xmat[1,  ] <- 0.7
	xmat[2,  ] <- 0.95
	xmat[3,  ] <- 0.95
	xmat[4,  ] <- 0.7
	xmat[5,  ] <- NA
	#	put  text in figure
	par(adj = 0)
	cont <- round(cont, digits = digits)
	if(!(label.resolution == "none")) {
		textx <- c(1:(length(cont) - 1))
		textx1 <- textx
		textx <- format(round(cont[1:(length(cont) - 1)] + 
			label.resolution, digits = digits))
		textx1 <- format(round(cont[2:length(cont)], digits = digits))
		textx <- paste(textx, "-", textx1)
		tmp1 <- paste(minsym, format(round(cont[1], digits = digits)))
		tmp2 <- paste(">", format(round(cont[ncont], digits = digits)))
		textx <- c(tmp1, textx, tmp2)
	}
	else {
		print(cont)
		textx <- c(1:length(cont))
		testx <- format(round(cont), digits = digits)
	}
	print(1)
	boxx <- c(matrix(leftrat, 1, length(boxy)))
	boxx <- xlim[1] + abs((xlim[2] - xlim[1])) * boxx
	boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
	ll <- (ylim[2] - ylim[1]) * 0.05
	if(!is.null(labtxt))
		textx <- labtxt
	# put the labels. 
	if(fill) text(boxx, boxy + ll/2, textx) else text(boxx, boxy + ll,
			textx)
	# put the labels. 
	xmat <- xlim[1] + abs((xlim[2] - xlim[1])) * xmat
	ymat <- ylim[1] + (ylim[2] - ylim[1]) * ymat
	if(label.resolution == "none") {
		colors <- colors[2:length(colors)]
	}
	polygon(xmat, ymat, border = T, col = colors)
	if(colors[1] == 0 || first.color.trans) {
		xmat <- c(xmat[1:4], xmat[1])
		# if white color.  
		ymat <- c(ymat[1:4], ymat[1])
		lines(xmat, ymat)
	}
}

labels2<-
function(cont, digits, colors, xlim = c(0, 1), ylim = c(0, 1), nx = 4, fill = F
	)
{
	xlim <- sort(xlim)
	ylim <- sort(ylim)
	ncont <- length(cont)
	lbox <- ncont + 1
	if(fill)
		lbox <- max(lbox, 20)
	boxy <- c(1:lbox)
	boxy <-  - boxy/(lbox + 2) + 1
	dy <- 1/lbox
	boxy <- boxy - dy/2
	boxy1 <- boxy + 1/lbox
	ymat <- matrix(0, 5, length(boxy))
	ymat[1,  ] <- boxy
	ymat[2,  ] <- boxy
	ymat[3,  ] <- boxy1
	ymat[4,  ] <- boxy1
	ymat[5,  ] <- NA
	xmat <- matrix(0, 5, length(boxy))
	xmat[1,  ] <- 0.6
	xmat[2,  ] <- 0.9
	xmat[3,  ] <- 0.9
	xmat[4,  ] <- 0.6
	xmat[5,  ] <- NA
	#	put  text in figure
	ind <- c(1, c(1:floor((length(cont))/nx)) * nx)
	if(ind[length(ind)] == (length(cont)))
		ind <- c(ind, (length(cont)))
	par(adj = 0)
	cont <- round(cont, digits = digits)
	textx <- format(round(cont[ind], digits = digits))
	boxx <- c(matrix(0.1, 1, length(boxy)))
	boxx <- xlim[1] + (xlim[2] - xlim[1]) * boxx
	boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
	text(boxx[ind], boxy[ind], textx)
	# put the lables.  
	xmat <- xlim[1] + abs((xlim[2] - xlim[1])) * xmat
	ymat <- ylim[1] + (ylim[2] - ylim[1]) * ymat
	polygon(xmat, ymat, border = F, col = colors)
	if(colors[1] == 0) {
		xmat <- c(xmat[1:4], xmat[1])
		# if white color.  
		ymat <- c(ymat[1:4], ymat[1])
		lines(xmat, ymat)
	}
}
shading1<-
function(cont, digits, colors, xlim = c(0, 1), ylim = c(0, 1), fill = F, angle,
	rotate, cex, rat, minsym = "<")
{
	xlim <- sort(xlim)
	ylim <- sort(ylim)
	if(cex != 0)
		par(cex = cex)
	ncont <- length(cont)
	if(fill)
		lbox <- max(ncont + 1, 20)
	else lbox <- ncont + 1
	boxy <- c(1:lbox)
	boxy <-  - boxy/lbox + 1
	boxy1 <- boxy + 1/(1.2 * lbox)
	if(fill) {
		boxy <- boxy[1:(ncont + 1)]
		boxy1 <- boxy1[1:(ncont + 1)]
	}
	ymat <- matrix(0, 5, length(boxy))
	ymat[1,  ] <- boxy
	ymat[2,  ] <- boxy
	ymat[3,  ] <- boxy1
	ymat[4,  ] <- boxy1
	ymat[5,  ] <- NA
	xmat <- matrix(0, 5, length(boxy))
	xmat[1,  ] <- 0.75
	xmat[2,  ] <- 0.97
	xmat[3,  ] <- 0.97
	xmat[4,  ] <- 0.75
	xmat[5,  ] <- NA
	#       put  text in figure
	par(adj = 0)
	cont <- round(cont, digits = digits)
	textx <- c(1:(length(cont) - 1))
	textx1 <- textx
	textx <- as.character(round(cont[1:(length(cont) - 1)], digits = digits
		))
	textx1 <- as.character(round(cont[2:length(cont)], digits = digits))
	textx <- paste(textx, "-", textx1)
	minsym <- paste(minsym, " ", sep = "")
	textx <- c(paste(minsym, as.character(round(cont[1], digits = digits))),
		textx)
	textx[ncont + 1] <- paste("> ", as.character(round(cont[ncont], digits
		 = digits)))
	boxx <- c(matrix(0.1, 1, length(boxy)))
	boxx <- xlim[1] + (xlim[2] - xlim[1]) * boxx
	boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
	ll <- (ylim[2] - ylim[1]) * 0.05
	if(fill)
		text(boxx, boxy + ll/2, textx)
	else text(boxx, boxy + ll, textx)
	xmat <- xlim[1] + (xlim[2] - xlim[1]) * xmat
	ymat <- ylim[1] + (ylim[2] - ylim[1]) * ymat
	for(i in 1:length(colors)) {
		polygon(xmat[1:4, i], ymat[1:4, i], border = T, density = 
			colors[i], angle = angle)
		angle <- angle + rotate
	}
}
geolocator<-
function(type = "p", n = 0)
{
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	if(n == 0)
		x <- locator(type = type)
	else x <- locator(type = type, n = n)
	if(!is.null(x$x)) {
		lat <- invProj(x$x, x$y, geopar$scale, geopar$b0, geopar$b1,
			geopar$l1, projection = geopar$projection)
		if(geopar$projection == "none")
			return(x <- data.frame(x = lat$x, y = lat$y))
		else return(lat <- data.frame(lat = lat$lat, lon = lat$lon))
	}
	else return(list())
}

arcdist<-
function(lat, lon, lat1 = NULL, lon1 = NULL, scale = "Miles")
{
	if(is.null(lat1)) {
		lat1 <- lon$lat
		lon1 <- lon$lon
		lon <- lat$lon
		lat <- lat$lat
	}
	if(scale == "Miles")
		miles <- 1.852
	else miles <- 1
	rad <- 6367
	#radius of earth in km
	mult1 <- (rad/miles)
	mult2 <- pi/180
	return(mult1 * acos(sin(mult2 * lat) * sin(mult2 * lat1) + cos(mult2 *
		lat) * cos(mult2 * lat1) * cos(mult2 * lon - mult2 * lon1)))
}
geolegend<-
function(pos, legend, ...)
{
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	xx <- Proj(pos$lat, pos$lon)
	legend(xx$x, xx$y, legend = legend, ...)
}

geocontour<-
function(grd, z, nlevels = 10, levels = NULL, labex = 1, triangles = T, reg = 0,
	fill = 1, colors = T, col = 1, only.positive = F, maxcol = 155, cex = 
	0.7, save = F, plotit = T, label.location = 0, lwd = 0, lty = 0, 
	labels.only = F, digits = 1, paint = F, set = NA, col.names = c("lon",
	"lat"),csi=NULL)
{
     if(!is.null(csi)) cex <- cex*csi/0.12 # Compatibility
	if(!is.null(attributes(grd)$grid)) {
		z <- grd
		grd <- attributes(grd)$grid
	}
	limits <- NULL
	maxn <- 10000
	grd <- Set.grd.and.z(grd, z, NULL, set, col.names)
	# Set data on correct form.
	z <- grd$z
	# Perturb z a little
	z <- z + rnorm(length(z)) * 1e-09
	grd <- grd$grd
	grd <- extract(grd, z, maxn, limits, col.names = col.names)
	# extract.   
	z <- grd$z
	grd <- grd$grd1
	# 	change NA to 0 or mean
	ind <- c(1:length(z))
	ind <- ind[is.na(z)]
	if(length(ind) > 0) {
		if(fill == 0)
			z[ind] <- -99999
		if(fill == 1)
			z[ind] <- 0
		if(fill == 2)
			z[ind] <- mean(z)
	}
	lon <- grd[[col.names[1]]]
	lat <- grd[[col.names[2]]]
	if(only.positive) {
		# put z<0 to 0
		ind <- c(1:length(z))
		ind <- ind[z < mean(z[z > 0])/1000 & z != -99999]
		z[ind] <- mean(z[z > 0])/1000
	}
	# Check if a setup from geoplot is to be used.  
	cond1 <- col.names[1] == "lon" && col.names[2] == "lat"
	cond2 <- col.names[1] == "x" && col.names[2] == "y" && geopar$
		projection == "none"
	if(cond1 || cond2) {
		oldpar <- selectedpar()
		on.exit(par(oldpar))
		par(geopar$gpar)
		if(geopar$cont)
			par(plt = geopar$contlines)
	}
	if(cex != 0)
		par(cex = cex)
	nx <- length(lon)
	ny <- length(lat)
	lon1 <- matrix(lon, nx, ny)
	lat1 <- t(matrix(lat, ny, nx))
	# Transform the matrices if lat,lon.  Proj only transforms if col.names=0
	if(!labels.only) {
		if(geopar$projection == "Mercator" && col.names[1] == "lon") {
			z <- matrix(z, nrow = length(lon), ncol = length(lat))
			lon2 <- c(matrix(lon[1], length(lat), 1))
			lat2 <- c(matrix(lat[1], length(lon), 1))
			xlat <- Proj(lat, lon2, geopar$scale, geopar$b0, geopar$
				b1, geopar$l1, geopar$projection)
			xlon <- Proj(lat2, lon, geopar$scale, geopar$b0, geopar$
				b1, geopar$l1, geopar$projection)
		}
		else {
			z <- matrix(z, nrow = length(lon), ncol = length(lat))
			xlon <- list(x = lon)
			xlat <- list(y = lat)
		}
	}
	if(colors) {
		# plot contour lines in colors
		if(is.null(levels)) {
			if(nlevels == 0)
				nlevels <- 10
			levels <- pretty(z, nlevels)
		}
		nlevels <- length(levels)
		if(length(lty) == length(levels) && length(levels) > 1)
			linetypes <- T
		else linetypes <- F
		if(length(lwd) == length(levels) && length(levels) > 1)
			linew <- T
		else linew <- F
		if(length(col) == 1) {
			if(length(lty) == length(levels))
				color <- rep(1, nlevels)
			else {
				mincol <- 2
				color <- c(1:nlevels)
				color <- round(2 + ((color - 1) * maxcol)/
					(nlevels))
			}
		}
		else color <- col
		if(!labels.only) {
			if(length(ind) > 1)
				z[ind] <- NA
			if(geopar$projection == "Lambert") {
				lev <- contour(lon + 400, lat, z, levels = 
					levels,  , triangles = triangles, save
					 = T, plotit = F)
				for(i in 1:length(lev)) {
					if(linew)
						lw <- lwd[i]
					else lw <- 0
					if(linetypes)
						lt <- lty[i]
					else lt <- 0
					geolines(lev[[i]]$y, lev[[i]]$x - 400,
						col = color[i], lty = lt, lwd
						 = lw)
				}
			}
			else {
				for(i in 1:nlevels) {
					if(linetypes)
						par(lty = lty[i])
					if(linew)
						par(lwd = lwd[i])
					lev <- contour(xlon$x, xlat$y, z, axes
						 = F, levels = c(levels[i],
						levels[i]), add = T, triangles
						 = triangles, labex = labex,
						xlim = geopar$limx, ylim = 
						geopar$limy, col = color[i],
						xlab = " ", ylab = " ", save = 
						F, plotit = T)
				}
			}
		}
	}
	else {
		# one color. 
		if(length(ind) > 1) z[ind] <- NA
		if(geopar$projection == "Lambert") {
			if(length(levels) == 1)
				lev <- contour(lon + 400, lat, z, nlevels = 
					nlevels, triangles = triangles, save = 
					T, plotit = F)
			else {
				lev <- contour(lon + 400, lat, z, axes = F,
					levels = levels, triangles = triangles,
					add = T, labex = labex, col = col,
					xlab = " ", ylab = " ", save = T, 
					plotit = F)
				# +400 v. galla
				for(i in 1:length(lev))
					geolines(lev[[i]]$y, lev[[i]]$x - 400,
						col = col)
			}
		}
		else {
			if(length(levels) == 1)
				lev <- contour(xlon$x, xlat$y, z, nlevels = 
					nlevels, triangles = triangles, labex
					 = labex, add = T, xlim = geopar$limx,
					ylim = geopar$limy, axes = F, col = col,
					xlab = " ", ylab = " ", save = save,
					plotit = plotit)
			else lev <- contour(xlon$x, xlat$y, z, axes = F, levels
					 = levels, triangles = triangles, add
					 = T, labex = labex, xlim = geopar$
					limx, ylim = geopar$limy, col = col,
					xlab = " ", ylab = " ", save = save,
					plotit = plotit)
		}
	}
	if(geopar$projection == "Lambert")
		par(geopar$gpar)
	if(length(label.location) == 1)
		if(label.location == "locator")
			label.location <- geolocator(n = 2)
	# use the locator.  
	if(length(label.location) > 1) {
		#label located somewhere in drawing
		label.location <- Proj(label.location, scale = geopar$scale,
			b0 = geopar$b0, b1 = geopar$b1, l1 = geopar$l1, 
			projection = geopar$projection)
		if(geopar$projection == "none")
			paint.window.x(label.location, border = T)
		else paint.window(label.location, border = T)
		labels.line(levels, digits, color, lty, xlim = label.location$
			x, ylim = label.location$y, linew)
	}
	if(geopar$cont && colors) {
		# if labels needed.  
		par(plt = geopar$contlab)
		par(new = T)
		plot(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0), type = "l", axes = F,
			xlab = " ", ylab = " ")
		labels.line(levels, digits, color, lty, linew)
	}
	#	Add borders
	if(length(reg) > 1 && paint) {
		nx <- length(lon)
		ny <- length(lat)
		lon <- matrix(lon, nx, ny)
		lat <- t(matrix(lat, ny, nx))
		shadeborder(reg, lat, lon)
	}
	if(cond1 || cond2) {
		par(oldpar)
	}
	if(save) {
		return(invisible(lev))
	}
	else return(invisible())
}

paint.window.x<-
function(listi, col = 0., border = T, poly = T)
{
	x <- list(y = c(listi$y[1.], listi$y[1.], listi$y[2.], listi$y[2.],
		listi$y[1.]), x = c(listi$x[1.], listi$x[2.], listi$x[2.],
		listi$x[1.], listi$x[1.]))
	rx <- range(x$x)
	ry <- range(x$y)
	t1 <- c(rx[1.], rx[2.], rx[2.], rx[1.], rx[1.])
	t2 <- c(ry[1.], ry[1.], ry[2.], ry[2.], ry[1.])
	if(border) {
		mx <- mean(t1[1.:4.])
		my <- mean(t2[1.:4.])
		t11 <- t1 + 0.02 * (t1 - mx)
		t22 <- t2 + 0.02 * (t2 - my)
		lines(t11, t22, lwd = 1.5, col = 1.)
	}
	if(poly)
		polygon(t1, t2, col = 0.)
}
paint.window<-
function(listi, col = 0, border = T, poly = T, col.names = c("lon", "lat"))
{
	lat <- c(listi[[col.names[2]]][1], listi[[col.names[2]]][1], listi[[
		col.names[2]]][2], listi[[col.names[2]]][2], listi[[col.names[
		2]]][1])
	lon <- c(listi[[col.names[1]]][1], listi[[col.names[1]]][2], listi[[
		col.names[1]]][2], listi[[col.names[1]]][1], listi[[col.names[
		1]]][1])
	x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection, col.names = col.names)
	rx <- range(x$x)
	ry <- range(x$y)
	t1 <- c(rx[1], rx[2], rx[2], rx[1], rx[1])
	t2 <- c(ry[1], ry[1], ry[2], ry[2], ry[1])
	if(poly)
		polygon(t1, t2, col = 0)
	if(border) {
		mx <- mean(t1[1:4])
		my <- mean(t2[1:4])
		t11 <- t1 + 0.02 * (t1 - mx)
		t22 <- t2 + 0.02 * (t2 - my)
		lines(t11, t22, lwd = 1.5, col = 1)
	}
}

shadeborder<-
function(reg, lat, lon, col = 0, col.names = c("lon", "lat"))
{
	ind <- c(1:length(reg[[col.names[2]]]))
	ind1 <- ind[is.na(reg[[col.names[2]]])]
	if(length(ind1) == 0 || ind1[1] != 1) {
		#external border does not begin with NA
		if(length(ind1) < 1) ind2 <- length(reg[[col.names[2]]]) else 
				ind2 <- ind1[1] - 1
		reg.lat <- reg[[col.names[2]]][1:ind2]
		reg.lon <- reg[[col.names[1]]][1:ind2]
		lonx <- c(min(lon), min(lon), max(lon), max(lon), min(lon),
			min(lon))
		latx <- c(mean(lat), min(lat), min(lat), max(lat), max(lat),
			mean(lat))
		ind2 <- ind[reg.lon == min(reg.lon)][1]
		ind3 <- ind[reg.lon == max(reg.lon)][1]
		ind6 <- ind[reg.lat == min(reg.lat)][1]
		ind7 <- ind[reg.lat == max(reg.lat)][1]
		i <- 0
		if(ind6 > ind2)
			i <- i + 1
		if(ind3 > ind6)
			i <- i + 1
		if(ind7 > ind3)
			i <- i + 1
		if(ind2 > ind7)
			i <- i + 1
		if(i > 1)
			ccw <- T
		else ccw <- F
		if(ccw) {
			#counterclockwise
			if(ind3 > ind2) {
				ind4 <- c(ind3:ind2)
				ind5 <- c(ind3:length(reg.lat), 1:ind2)
			}
			else {
				ind4 <- c(ind3:1, length(reg.lat):ind2)
				ind5 <- c(ind3:ind2)
			}
		}
		else {
			#clockwise
			if(ind3 > ind2) {
				ind4 <- c(ind3:length(reg.lat), 1:ind2)
				ind5 <- c(ind3:ind2)
			}
			else {
				ind4 <- c(ind3:ind2)
				ind5 <- c(ind3:1, length(reg.lat):ind2)
			}
		}
		mil <- min(min(lon), min(reg.lon) - 1)
		mal <- max(max(lon), max(reg.lon) + 1)
		rlat <- c(mean(lat), min(lat), min(lat), mean(lat))
		rlon <- c(mil, mil, mal, mal)
		rlon <- c(reg.lon[ind4], rlon)
		rlat <- c(reg.lat[ind4], rlat)
		rx <- Proj(rlat, rlon, geopar$scale, geopar$b0, geopar$b1,
			geopar$l1, geopar$projection, col.names = col.names)
		lines(rx, lwd = 2)
		#    polygon(rx$x, rx$y, border = F, col = col)
		rlat <- c(mean(lat), max(lat), max(lat), mean(lat))
		rlon <- c(mil, mil, mal, mal)
		rlon <- c(reg.lon[ind5], rlon)
		rlat <- c(reg.lat[ind5], rlat)
		rx <- Proj(rlat, rlon, geopar$scale, geopar$b0, geopar$b1,
			geopar$l1, geopar$projection, col.names = col.names)
		lines(rx, lwd = 2, col = 70)
		#    polygon(rx$x, rx$y, border = F, col = col)
		if(length(ind1) > 0) {
			if(geopar$projection == "none") {
				if(length(reg$x) - ind1[length(ind1)] < 3)
					return(invisible())
				reg$x <- reg$x[(ind1[1] + 1):length(reg$x)]
				reg$y <- reg$y[(ind1[1] + 1):length(reg$y)]
			}
			else {
				if(length(reg[[col.names[2]]]) - ind1[length(
					ind1)] < 3)
					return(invisible())
				reg[[col.names[2]]] <- reg[[col.names[2]]][
					(ind1[1] + 1):length(reg[[col.names[
					2]]])]
				reg[[col.names[1]]] <- reg[[col.names[1]]][
					(ind1[1] + 1):length(reg[[col.names[
					1]]])]
			}
			rx <- Proj(reg, scale = geopar$scale, b0 = geopar$
				b0, b1 = geopar$b1, l1 = geopar$l1, projection
				 = geopar$projection, col.names = col.names)
			lines(rx, lwd = 2, col = 2)
		}
	}
	else {
		rx <- Proj(reg, scale = geopar$scale, b0 = geopar$b0, b1 = 
			geopar$b1, l1 = geopar$l1, projection = geopar$
			projection, col.names)
		lines(rx, lwd = 2, col = 150)
	}
}
geoexpand<-
function(grid)
{
	if(is.null(grid$lat)) {
		ny <- length(grid$y)
		nx <- length(grid$x)
		y <- t(matrix(grid$y, ny, nx))
		x <- matrix(grid$x, nx, ny)
		return(data.frame(y = c(y), x = c(x)))
	}
	else {
		nlat <- length(grid$lat)
		nlon <- length(grid$lon)
		lat <- t(matrix(grid$lat, nlat, nlon))
		lon <- matrix(grid$lon, nlon, nlat)
		return(data.frame(lat = c(lat), lon = c(lon)))
	}
}
Set.grd.and.z<-
function(grd, z, mask, set = NA, col.names = c("lon", "lat"))
{
	# z is a name of a column in the dataframe grd.
	if(is.data.frame(grd) && is.character(z)) z <- grd[, z]
	if(is.data.frame(grd) && nrow(grd) == length(z)) {
		i1 <- match(col.names[1], names(grd))
		i2 <- match(col.names[2], names(grd))
		xgr <- sort(unique(grd[, i1]))
		ygr <- sort(unique(grd[, i2]))
		xgr.1 <- c(matrix(xgr, length(xgr), length(ygr)))
		ygr.1 <- c(t(matrix(ygr, length(ygr), length(xgr))))
		xgr.data <- data.frame(x = xgr.1, y = ygr.1)
		names(xgr.data) <- col.names
		xgr.data$z <- rep(set, nrow(xgr.data))
		index <- paste(xgr.data[, 1], xgr.data[, 2], sep = "-")
		index1 <- paste(grd[, i1], grd[, i2], sep = "-")
		j <- match(index1, index)
		xgr.data$z[j] <- z
		grd1 <- list(xgr, ygr)
		names(grd1) <- col.names
		return(list(grd = grd1, z = xgr.data$z))
	}
	# grd is a list like returned by pointkriging
	if(is.list(grd) && !is.data.frame(grd)) {
		i1 <- match(col.names[1], names(grd))
		i2 <- match(col.names[2], names(grd))
		xgr <- grd[[i1]]
		ygr <- grd[[i2]]
		if(length(xgr) * length(ygr) != length(z)) {
			cat("Incorrect length on z")
			return(invisible())
		}
		xgr.1 <- c(matrix(xgr, length(xgr), length(ygr)))
		ygr.1 <- c(t(matrix(ygr, length(ygr), length(xgr))))
		xgr.data <- data.frame(x = xgr.1, y = ygr.1)
		names(xgr.data) <- col.names
		xgr.data$z <- z
		grd1 <- list(xgr, ygr)
		names(grd1) <- col.names
		return(list(grd = grd1, z = xgr.data$z))
	}
}
extract<-
function(grd, z, maxn = 10000, limits = NULL, col.names = c("lon", "lat"))
{
	if(is.null(limits)) {
		if(col.names[1] == "lon" && col.names[2] == "lat") {
			if(geopar$projection == "Lambert") {
				# complicated borders in lat,lon
				p1 <- list(x = c(geopar$limx[1], mean(geopar$
					limx), geopar$limx[1], geopar$limx[
					2]), y = c(geopar$limy[1], geopar$
					limy[2], geopar$limy[2], geopar$limy[
					2]))
				limits <- invProj(p1$x, p1$y, geopar$scale,
					geopar$b0, geopar$b1, geopar$l1, geopar$
					projection)
				xlim <- c(limits$lon[3], limits$lon[4])
				ylim <- c(limits$lat[1], limits$lat[2])
				limits <- list(lon = xlim, lat = ylim)
			}
			else {
				limits <- invProj(geopar$limx, geopar$limy,
					geopar$scale, geopar$b0, geopar$b1,
					geopar$l1, geopar$projection)
				xlim <- c(limits$lon[1], limits$lon[2])
				ylim <- c(limits$lat[1], limits$lat[2])
				limits <- list(lon = xlim, lat = ylim)
			}
		}
		else {
			limits <- list(x = par()$usr[1:2], y = par()$usr[3:
				4])
			names(limits) <- col.names
		}
	}
	ind10 <- c(1:length(grd[[col.names[1]]]))
	ind1 <- ind10[grd[[col.names[1]]] >= limits[[col.names[1]]][1] & grd[[
		col.names[1]]] <= limits[[col.names[1]]][2]]
	ind20 <- c(1:length(grd[[col.names[2]]]))
	ind2 <- ind20[grd[[col.names[2]]] >= limits[[col.names[2]]][1] & grd[[
		col.names[2]]] <= limits[[col.names[2]]][2]]
	ind10 <- matrix(ind10, length(ind10), length(ind20))
	ind20 <- t(matrix(ind20, length(ind20), nrow(ind10)))
	ind <- c(1:length(ind10))
	if(length(ind1) * length(ind2) > maxn) {
		if(col.names[1] == "lon" && col.names[2] == "lat") {
			rat <- cos((mean(limits[[col.names[2]]]) * pi)/180)
			nlat <- (limits[[col.names[2]]][2] - limits[[col.names[
				2]]][1])
			nlon <- (limits[[col.names[1]]][2] - limits[[col.names[
				1]]][1]) * rat
			rat <- nlat/nlon
			nlat <- sqrt(maxn * rat)
			nlon <- sqrt(maxn/rat)
			ind1 <- seq(min(ind1), max(ind1), by = round(length(
				ind1)/nlon))
			ind2 <- seq(min(ind2), max(ind2), by = round(length(
				ind2)/nlat))
		}
		else {
			rat <- maxn/(length(ind1) * length(ind2))
			nlat <- length(ind2) * sqrt(rat)
			nlon <- length(ind1) * sqrt(rat)
			ind1 <- seq(min(ind1), max(ind1), by = round(length(
				ind1)/nlon))
			ind2 <- seq(min(ind2), max(ind2), by = round(length(
				ind2)/nlat))
		}
	}
	grd1 <- list(grd[[col.names[1]]][ind1], grd[[col.names[2]]][ind2])
	names(grd1) <- col.names
	ind <- ind[!is.na(match(ind10, ind1)) & !is.na(match(ind20, ind2))]
	z <- z[ind]
	return(list(grd1 = grd1, z = z))
}
labels.line<-
function(cont, digits, colors, lty, xlim = c(0, 1), ylim = c(0, 1), linew = F)
{
	xlim <- sort(xlim)
	ylim <- sort(ylim)
	ncont <- length(cont)
	if(length(lty) == ncont)
		linetypes <- T
	else linetypes <- F
	lbox <- ncont
	boxy <- c(1:lbox)
	boxy <-  - boxy/(lbox + 1) + 1
	boxy1 <- boxy + 1/(1.2 * lbox)
	ymat <- matrix(0, 2, length(boxy))
	ymat[1,  ] <- boxy
	ymat[2,  ] <- boxy
	xmat <- matrix(0, 2, length(boxy))
	xmat[1,  ] <- 0.7
	xmat[2,  ] <- 0.95
	#	put  text in figure
	par(adj = 0)
	cont <- round(cont, digits = digits)
	textx <- format(cont)
	boxx <- c(matrix(0.1, 1, length(boxy)))
	boxx <- xlim[1] + abs((xlim[2] - xlim[1])) * boxx
	boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
	ll <- (ylim[2] - ylim[1]) * 0.04
	text(boxx, boxy + ll, textx, col = 1)
	# put the lables.  
	xmat <- xlim[1] + abs((xlim[2] - xlim[1])) * xmat
	ymat <- ylim[1] + (ylim[2] - ylim[1]) * ymat
	for(i in 1:ncont) {
		if(linew)
			par(lwd = lwd[i])
		if(linetypes)
			par(lty = lty[i])
		lines(xmat[, i], ymat[, i] + ll, col = colors[i])
	}
}


pointkriging<-
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
bua<-
function(nm = 10)
{
	rrt <- c(0, 0, 1, 1)
	crt <- c(0, 1, 1, 0)
	for(i in 2:nm) {
		stdrrt <- c(matrix(0, 8 * i - 4, 1))
		stdcrt <- stdrrt
		n <- i * 8 - 4
		stdrrt[1:(2 * i)] <- 1 - i
		stdrrt[(2 * i + 1):(4 * i - 1)] <- c((2 - i):i)
		stdrrt[(4 * i - 1):(6 * i - 2)] <- i
		stdrrt[(6 * i - 2):(8 * i - 4)] <- c(i:(2 - i))
		stdcrt[1:(2 * i)] <- (c(1 - i):i)
		stdcrt[(2 * i + 1):(4 * i - 1)] <- i
		stdcrt[(4 * i - 1):(6 * i - 2)] <- c(i:(1 - i))
		stdcrt[(6 * i - 2):(8 * i - 4)] <- 1 - i
		crt <- c(crt, stdcrt)
		rrt <- c(rrt, stdrrt)
	}
	i1 <- 4
	for(i in 2:nm) {
		i1[i] <- i1[i - 1] + 8 * i - 4
	}
	i1 <- c(0, i1)
	# 	Part that comes instead of the loop that
	#	is too slow.  
	ind <- c(3, 0, 2, 0, 4, 0, 1)
	r1 <- rrt - 0.1
	c1 <- crt - 0.1
	rr <- sign(r1) + sign(c1) * 2 + 4
	dir <- ind[rr]
	#	dir<- c(1:length(rrt))
	#	for(i in 1:length(rrt)){
	#		if(rrt[i]>0 && crt[i]>0)dir[i]<-1
	#		if(rrt[i]<= 0 && crt[i]>0)dir[i]<-4
	#		if(rrt[i]<= 0 && crt[i]<=0 )dir[i]<-3
	#		if(rrt[i]>0 && crt[i]<=0)dir[i]<-2
	#	}
	return(list(rrt = rrt, crt = crt, dir = dir, i1 = i1))
}

geocontour.fill<-
function(grd, z, levels = NULL, nlevels = 0, cex =0.7, digits = 1, col = NULL,
	working.space = 0, labels = 1, ratio = 1000, only.positive = F, fill = 
	0, maxcol = 155, white = F, label.location = 0, labels.only = F, 
	bordercheck = F, maxn = 10000, bcrat = 0.05, limits = NULL, col.names
	 = c("lon", "lat"), minsym = "<", label.resolution = 0, labtxt = NULL,
	boxcol = 0, first.color.trans = T, mai = c(0, 1, 0, 1), leftrat = 0.1,
	labbox = T,csi=NULL)
{
     if(!is.null(csi)) cex <- cex*csi/0.12 # Compatibility
	if(!is.null(attributes(grd)$grid)) { 
		z <- grd
		grd <- attributes(grd)$grid
	}
	set <- NA
	fact <- 2
	grd <- Set.grd.and.z(grd, z, NULL, set, col.names)
	# Set data on correct form.
	z <- grd$z
	# Perturb z a little
	z <- z + rnorm(length(z)) * 1e-09
	grd <- grd$grd
	if(is.null(levels)) {
		# changed before cont < 2  
		if(nlevels == 0) nlevels <- 10
		levels <- pretty(range(z, na.rm = T), nlevels)
		levels <- levels[2:(length(levels) - 1)]
	}
	ncont <- length(levels)
	#	Set colors if needed
	if(is.null(col)) {
		if(white) {
			# lowest values white.  
			mincol <- 2
			colors <- c(1:(ncont))
			colors <- floor(mincol + ((colors - 1) * (maxcol - 
				mincol))/(length(colors) - 1))
			colors <- c(0, colors)
		}
		else {
			mincol <- 2
			colors <- c(1:(ncont + 1))
			colors <- floor(mincol + ((colors - 1) * (maxcol - 
				mincol))/(length(colors) - 1))
		}
	}
	else colors <- col
	levels.1 <- levels
	colors.1 <- colors
	m <- max(z[!is.na(z)])
	if(!is.null(levels)) {
		i <- c(1:length(levels))
		i <- i[levels > max(z[!is.na(z)])]
		if(length(i) > 0) {
			levels <- levels[ - i]
			if(length(colors) > 1) {
				i <- i + 1
				colors <- colors[ - i]
			}
		}
	}
	ncont <- nlevels <- length(levels)
	cont <- levels
	# change names of variables
	grd <- extract(grd, z, maxn, limits, col.names = col.names)
	# extract.   
	z <- grd$z
	grd <- grd$grd1
	ind <- c(1:length(z))
	ind <- ind[is.na(z)]
	if(length(ind) > 0) {
		if(fill == 0)
			z[ind] <- -99999
		if(fill == 1)
			z[ind] <- 0
		if(fill == 2)
			z[ind] <- mean(z)
	}
	lon <- grd[[col.names[1]]]
	lat <- grd[[col.names[2]]]
	if(only.positive) {
		# put z<0 to 0
		ind <- c(1:length(z))
		ind <- ind[z < mean(z[z > 0])/1000 & z != -99999]
		z[ind] <- mean(z[z > 0])/1000
	}
	# Check if a setup from geoplot is to be used.  
	cond1 <- col.names[1] == "lon" && col.names[2] == "lat"
	cond2 <- col.names[1] == "x" && col.names[2] == "y" && geopar$
		projection == "none"
	if(cond1 || cond2) {
		oldpar <- selectedpar()
		on.exit(par(oldpar))
		par(geopar$gpar)
		if(geopar$cont)
			par(plt = geopar$contlines)
	}
	if(cex != 0)
		par(cex = cex)
	nx <- length(lon)
	ny <- length(lat)
	mcont <- mean( - cont[1:(ncont - 1)] + cont[2:(ncont)])
	lon1 <- matrix(lon, nx, ny)
	lat1 <- t(matrix(lat, ny, nx))
	# Transform the matrices if lat,lon.  Proj only transforms if col.names=0
	if(col.names[1] == "lon" & col.names[2] == "lat") {
		x1 <- Proj(lat1, lon1, geopar$scale, geopar$b0, geopar$b1,
			geopar$l1, geopar$projection, col.names)
		y1 <- x1$y
		x1 <- x1$x
	}
	else {
		# no projection.
		x1 <- lon1
		y1 <- lat1
	}
	# Check what is inside borders if given.  
	cutreg <- F
	lx <- length(x1)
	x1 <- x1 + rnorm(lx)/1000000
	inni <- 0
	indd <- c(0, 1, 4, 1, 2, 4, 2, 3, 4, 3, 0, 4)
	cont <- c(cont, max(c(max(abs(cont)) * 1.1, max(z) * 1.1)))
	# change.
	cont <- c(min(c(min(z[z != -99999]) - 1, cont[1] - 1)), cont)
	if(cont[2] - cont[1] < 1)
		cont[1] <- cont[2] - 1
	ncont <- ncont + 2
	if(!labels.only) {
		nel <- (nx - 1) * (ny - 1)
		el <- matrix(0, nel, 4)
		err <- 0
		# error
		#    flag
		if(working.space == 0) {
			# Try to calculate working space
			z1 <- z[z > -99998]
			zm <- mean(abs(z1[3:(length(z1) - 1)] - z1[2:(length(
				z1) - 2)]))
			cm <- mean(abs(cont[3:(length(cont) - 1)] - cont[2:
				(length(cont) - 2)]))
			nel <- (nx - 1) * (ny - 1) * 4
			working.space <- round(nel * (zm/cm + 3) * fact * 2)/
				2
			if(working.space <= 20000)
				working.space <- 20000
			# maximum lenght of contour lines
			if(working.space > 200000) working.space <- 10 * length(
					x1)
		}
		polyx <- c(1:working.space)
		polyx[1:working.space] <- 0
		polyy <- c(1:working.space)
		polyy[1:working.space] <- 0
		print(paste("calculated working space is", working.space))
		polyy <- .C("elcont",
			as.double(c(x1)),
			as.double(c(y1)),
			as.double(c(z)),
			as.integer(lx),
			as.integer(nx),
			as.integer(ny),
			as.double(cont),
			as.integer(ncont),
			as.double(polyx),
			as.double(polyy),
			as.integer(el),
			as.integer(nel),
			as.integer(working.space),
			as.integer(err),
			as.integer(inni),
			as.integer(cutreg),
			as.integer(indd),
			as.integer(white))
		err <- polyy[[14]]
		if(err == 1) {
			print("error, working.space not big enough, try calling program again with bigger working.space"
				)
			return(invisible())
		}
		polyx <- polyy[[9]]
		polyy <- polyy[[10]]
		ind <- c(1:length(polyy))
		ind <- ind[polyy > 90000]
		indmax <- ind[length(ind)]
		print(paste(" used working space is ", indmax))
		polyy[ind] <- NA
		col <- polyx[ind]
		# find the color
		polyx[ind] <- NA
		# between polygons.  
		polyx <- polyx[1:indmax]
		polyy <- polyy[1:indmax]
		#		Finding the colors.  
		cont <- cont + 1e-05
		# numerical problem
		ncol <- cut(col, cont,labels=FALSE)  # labels=FALSE R ver.
		ncol <- colors[ncol]
		ind <- c(1:length(ncol))
		ind <- ind[is.na(ncol)]
		ncol[ind] <- 0
		if(bordercheck) {
			eps <- 0.0001
			lx <- geopar$limx - bcrat * (geopar$limx - mean(geopar$
				limx))
			ly <- geopar$limy - bcrat * (geopar$limy - mean(geopar$
				limy))
			lx[1] <- lx[1] - eps
			lx[2] <- lx[2] + eps
			ly[1] <- ly[1] - eps
			ly[2] <- ly[2] + eps
			ind <- c(1:length(polyx))
			ind <- ind[is.na(polyx) | (polyx < lx[2] & polyx > lx[
				1] & polyy < ly[2] & polyy > ly[1])]
			polyx <- polyx[ind]
			polyy <- polyy[ind]
			n <- length(polyx)
			ind <- c(1:length(polyx))
			ind <- ind[!is.na(polyx)]
			if(ind[1] != 1) {
				ind2 <- c(1:(ind[1] - 1))
				polyx <- polyx[ - ind2]
				polyy <- polyy[ - ind2]
				ncol <- ncol[ - ind2]
			}
			ind <- c(1:length(polyx))
			ind <- ind[is.na(polyx)]
			ind1 <- c(1:length(ind))
			ind1 <- ind1[diff(ind) == 1]
			ind2 <- ind[diff(ind) == 1]
			if(length(ind1) > 0) {
				ncol <- ncol[ - (ind1 + 1)]
			}
		}
		polygon(polyx, polyy, col = ncol, border = F)
	}
	# 	Add  labels around plot
	if(length(label.location) == 1) if(label.location == "locator") {
			# use the locator.
			if(cond1 | cond2) label.location <- geolocator(n = 2)
				 else label.location <- locator(n = 2)
		}
	if(length(label.location) > 1) {
		#label located somewhere in drawing
		if(labbox) paint.window(Proj(label.location, col.names = 
				col.names), border = T, col.names = c("y",
				"x"), col = boxcol)
		label.location <- Proj(label.location, scale = geopar$scale,
			b0 = geopar$b0, b1 = geopar$b1, l1 = geopar$l1, 
			projection = geopar$projection, col.name = col.names)
		if(labels == 1) {
			# labels for each contour line.  
			labels1(levels.1, digits, colors.1, xlim = 
				label.location$x, ylim = label.location$y,
				minsym = minsym, label.resolution = 
				label.resolution, labtxt = labtxt, 
				first.color.trans = first.color.trans, mai = 
				mai, leftrat = leftrat)
		}
		else {
			#more of a constant label. 
			labels2(levels.1, digits, colors.1, xlim = 
				label.location$x, ylim = label.location$y)
		}
	}
	if(geopar$cont && labels != 0) {
		# if labels needed.  
		par(plt = geopar$contlab)
		par(new = T)
		plot(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0), type = "l", axes = F,
			xlab = " ", ylab = " ")
		if(labels == 1) {
			# labels for each contour line.  
			labels1(levels.1, digits, colors.1, fill = geopar$
				cont, minsym = minsym, label.resolution = 
				label.resolution, labtxt = labtxt, 
				first.color.trans = first.color.trans, mai = 
				mai, leftrat = leftrat)
		}
		else {
			#more of a constant label. 
			labels2(levels.1, digits, colors.1, fill = geopar$
				cont)
		}
	}
	return(invisible())
}

combine.rt<-
function(lat, lon, z, grlat, grlon = 0, fun, fill = F, reg = 0, minnumber = 2,
	wsp = 0, wz = 0, wlat = 0, xy = F, rat = 0.2, type)
{
	if(missing(fun) && !missing(type))
		fun <- type
	# for compatibility
	if(!missing(fun) && fun == "summa") fun <- "sum"
	# also for compatibility
	if(xy) {
		if(length(grlon) < 2) {
			grlon <- grlat$y
			grlat <- grlat$x
		}
	}
	else {
		if(length(grlon) < 2) {
			grlon <- grlat$lon
			grlat <- grlat$lat
		}
	}
	ndata <- length(lat)
	if(length(wz) != ndata)
		wz <- rep(1, ndata)
	if(length(wlat) != ndata)
		wlat <- rep(1, ndata)
	n <- length(grlon)
	m <- length(grlat)
	row <- cut(lat, grlat,labels=FALSE)  # R ver
	col <- cut(lon, grlon,labels=FALSE)  # R ver
	reitur <- (n - 1) * (row - 1) + col
	ind <- c(1:length(reitur))
	ind <- ind[!is.na(reitur)]
	lat <- lat[ind]
	lon <- lon[ind]
	z <- z[ind]
	wlat <- wlat[ind]
	wz <- wz[ind]
	reitur <- reitur[ind]
	maxrt <- (n - 1) * (m - 1)
	grdlat <- (grlat[1:(m - 1)] + grlat[2:m])/2
	grdlon <- (grlon[1:(n - 1)] + grlon[2:n])/2
	#	what to do 
	if(fun == "mean") option <- 1
	if(fun == "sum")
		option <- 2
	if(fun == "median")
		option <- 3
	if(fun == "variance")
		option <- 4
	if(fun == "rm.outliers")
		option <- 5
	if(fun == "keep.all")
		option <- 6
	#	Fill up matrix of data.  
	pts.in.reit <- c(matrix(0, round(ndata * 1.2), 1))
	npts.in.reit <- jrt <- indrt <- rep(0, maxrt + 1)
	nnewlat <- 0
	if(option == 5) {
		newlat <- newlon <- newz <- newn <- fylla <- rep(0, length(
			lat))
	}
	else if(option == 6) {
		newlat <- newlon <- newz <- newn <- fylla <- rep(0, (length(
			lat) + length(grlat) * length(grlon)) * 1.1)
	}
	else {
		newlat <- newlon <- newz <- newn <- fylla <- rep(0, maxrt +
			1)
	}
	if(wsp == 0)
		wsp <- ndata
	workspace <- order <- nr <- rep(0, wsp)
	outcome <- .C("combinert",
		as.double(lat),
		as.double(lon),
		as.double(z),
		as.integer(length(z)),
		as.integer(reitur),
		as.integer(pts.in.reit),
		as.integer(npts.in.reit),
		as.integer(indrt),
		as.integer(jrt),
		as.integer(maxrt),
		as.double(newlat),
		as.double(newlon),
		as.double(newz),
		as.integer(newn),
		as.integer(nnewlat),
		as.integer(option),
		as.integer(fill),
		as.integer(fylla),
		as.double(grdlat),
		as.double(grdlon),
		as.integer(n),
		as.integer(minnumber),
		as.double(workspace),
		as.integer(nr),
		as.integer(order),
		as.double(wlat),
		as.double(rat),
		as.double(wz))
	nnewlat <- outcome[[15]]
	newn <- outcome[[14]][1:nnewlat]
	newlat <- outcome[[11]][1:nnewlat]
	newlon <- outcome[[12]][1:nnewlat]
	newz <- outcome[[13]][1:nnewlat]
	fylla <- outcome[[18]][1:nnewlat]
	if(xy)
		projection <- "none"
	else projection <- "Mercator"
	if(length(reg) > 1) {
		inni <- inside(newlat, newlon, reg, option = 0, projection = 
			projection)
		ind <- c(1:length(inni))
		ind <- ind[inni == 1]
		newlat <- newlat[ind]
		newlon <- newlon[ind]
		newz <- newz[ind]
		newn <- newn[ind]
	}
	if(option == 5)
		fylla <- 0
	# not used
	if(xy) z <- list(x = newlat, y = newlon, z = newz, n = newn, fill = 
			fylla) else z <- list(lat = newlat, lon = newlon, z = 
			newz, n = newn, fill = fylla)
	z <- data.frame(z)
	attributes(z)$fun <- fun
	return(invisible(z))
}
inside<-
function(lat, lon = 0., reg, option = 1., projection = "Mercator")
{
	# temporary copy
	tmp <- lat
	if(length(lon) < 2. & length(lat) >= 2.) {
		if(projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	if(length(reg$lat) == 2. && length(reg$lon) == 2.) {
		# rectangular limits 2 pts.
		la <- range(reg$lat)
		lo <- range(reg$lon)
		reg <- list(lat = c(la[1.], la[1.], la[2.], la[2.], la[1.]),
			lon = c(lo[1.], lo[2.], lo[2.], lo[1.], lo[1.]))
	}
	n <- length(reg$lat)
	# has to be closed
	if(n > 0.) if(reg$lat[1.] != reg$lat[n] | reg$lon[1.] != reg$lon[n]) {
			reg$lat <- c(reg$lat, reg$lat[1.])
			reg$lon <- c(reg$lon, reg$lon[1.])
		}
	indx <- c(1.:length(lat))
	indx <- indx[is.na(lat)]
	#outside borders. 
	if(length(indx) > 0.) {
		lat[indx] <- 70.
		lon[indx] <- -60.
	}
	if(projection == "none")
		border <- adapt(reg$x, reg$y)
	else border <- adapt(reg$lat, reg$lon)
	inni <- rep(0., length(lat))
	a <- a1 <- rep(0., length(reg$lat))
	if(!is.loaded(symbol.C("marghc1")))
		dyn.load(paste(DYN.HOME, "margh.new.o", sep = ""))
	inni <- .C("marghc1",
		as.single(lon),
		as.single(lat),
		as.integer(length(lat)),
		as.single(border$lon),
		as.single(border$lat),
		as.integer(length(border$lat)),
		as.integer(border$lxv),
		as.integer(length(border$lxv)),
		as.integer(inni),
		as.single(a),
		as.single(a1))
	inni <- inni[[9.]]
	ind <- c(1.:length(inni))
	ind <- ind[inni > 0.]
	if(option == 1.) {
		if(is.data.frame(tmp)) {
			tmp <- tmp[ind,  ]
			return(tmp)
		}
		else {
			lat <- lat[ind]
			lon <- lon[ind]
			if(projection == "none")
				return(x = lat, y = lon)
			else return(lat, lon)
		}
	}
	else if(option == 2.) {
		if(is.data.frame(tmp)) {
			tmp <- tmp[ - ind,  ]
			return(tmp)
		}
		else {
			lat <- lat[ - ind]
			lon <- lon[ - ind]
			if(projection == "none")
				return(x = lat, y = lon)
			else return(lat, lon)
		}
	}
	else if(option == 3.)
		return(inni)
	else if(option == 5.) {
		ind <- c(1.:length(inni))
		ind <- ind[inni == 0.]
		return(ind)
	}
	else return(ind)
}

geoidentify<-
function(lat, lon = NULL, labels = 1, n = 0, plot = T, atpen = T, offset = 0.5,
	col = 1, cex = 1)
{
	oldpar <- selectedpar()
	par(geopar$gpar)
	par(cex = cex)
	par(col = col)
	on.exit(par(oldpar))
	if(is.null(lon)) {
		if(geopar$projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	if(geopar$projection != "none") {
		# degrees and minutes
		if(mean(lat, na.rm = T) > 1000) {
			lat <- geoconvert(lat)
			lon <-  - geoconvert(lon)
		}
	}
	if(length(labels) == 1 && length(lat) > 1)
		labels <- seq(along = lat)
	if(n == 0)
		n <- length(lat)
	xx <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection)
	z <- identify(xx$x, xx$y, labels = labels, n = n, plot = plot, atpen = 
		atpen, offset = offset)
	return(z)
}

geodezoom<-
function()
{
	if(as.character(geopar$command[length(geopar$command)]) == "123")
		com <- geopar$command[1:(length(geopar$command) - 1)]
	else com <- geopar$command
	eval(com)
}

# Had earlier the name grid that was masked by a native function
setgrid<-
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
gridpoints<-
function(border, dx, grpkt, nx, n)
{
	if(length(grpkt) == 1) {
		# gridpoints not given.  
		if(geopar$projection == "none") {
			xmin <- min(border$x)
			xmax <- max(border$x)
			ymin <- min(border$y)
			ymax <- max(border$y)
		}
		else {
			xmin <- min(border$lon)
			xmax <- max(border$lon)
			ymin <- min(border$lat)
			ymax <- max(border$lat)
			meanlat <- (mean(border$lat) * pi)/180
		}
		if(dx[1] == 0) {
			if(nx[1] == 0) {
				n <- sqrt(n)
				if(geopar$projection == "none")
					k <- (xmax - xmin)/(ymax - ymin)
				else k <- ((xmax - xmin) * cos(meanlat))/(
						ymax - ymin)
				nx[1] <- round(n * sqrt(k))
				nx[2] <- round(n/sqrt(k))
			}
			dx[1] <- (xmax - xmin)/nx[1]
			dx[2] <- (ymax - ymin)/nx[2]
		}
		else {
			tmp <- dx[1]
			dx[1] <- dx[2]
			dx[2] <- tmp
			#exchange lat lon.  
			nx[1] <- trunc((xmax - xmin)/dx[1])
			nx[2] <- trunc((ymax - ymin)/dx[2])
		}
		xgr <- (xmin - dx[1]) + c(1:(nx[1] + 2)) * dx[1]
		ygr <- (ymin - dx[2]) + c(1:(nx[2] + 2)) * dx[2]
	}
	else if(projection == "none") {
		xgr <- grpkt$x
		ygr <- grpkt$y
	}
	else {
		xgr <- grpkt$lon
		ygr <- grpkt$lat
	}
	lx <- length(xgr)
	ly <- length(ygr)
	xgra <- c(matrix(xgr, lx, ly))
	ygra <- c(t(matrix(ygr, ly, lx)))
	if(geopar$projection == "none") {
		xgra <- list(x = xgra, y = ygra)
		xgr <- list(x = xgr, y = ygr)
	}
	else {
		xgra <- list(lon = xgra, lat = ygra)
		xgr <- list(lon = xgr, lat = ygr)
	}
	return(list(xgr = xgr, xgra = xgra))
}

giveborder<-
function(nholes = 0)
{
	cat(" Give the border of estimating area (not holes) : \n")
	xb <- locator(type = "l")
	lines(c(xb$x[length(xb$x)], xb$x[1]), c(xb$y[length(xb$x)], xb$y[1]))
	# last to fyrst point
	xba <- xb
	# vector to use for plotting
	xba$x <- c(xb$x, xb$x[1])
	#new
	xba$y <- c(xb$y, xb$y[1])
	#new
	#	Read in the data for holes.  Stored in two vectors (added to
	#	the border.  xba has NA's and the first point of each point
	#	twice and is used for plotting while xb is used for the
	#	program "margh" and has each point only once and no NA's.
	lxv <- c(1:(nholes + 2))
	lxv[] <- 0
	if(nholes > 0) {
		lx1 <- 1
		#index
		for(i in 1:nholes) {
			xba$x <- c(xba$x, xb$x[lx1], NA)
			# vector to use in
			xba$y <- c(xba$y, xb$y[lx1], NA)
			# contourplots first point	    
			cat("Hole (or other area) ", i, "\n")
			lx1 <- length(xb$x) + 1
			# preserve last length
			lxv[i + 1] <- length(xb$x)
			xb1 <- locator(type = "l")
			xb$x <- c(xb$x, xb1$x)
			xb$y <- c(xb$y, xb1$y)
			xba$x <- c(xba$x, xb1$x)
			xba$y <- c(xba$y, xb1$y)
			lx2 <- length(xb$x)
			xba$x <- c(xba$x, xb$x[lx1])
			xba$y <- c(xba$y, xb$y[lx1])
			#new
			lines(c(xb$x[lx2], xb$x[lx1]), c(xb$y[lx2], xb$y[lx1]))
		}
	}
	if(nholes == 0)
		lx1 <- 1
	#  	xba$x <-c(xba$x,xb$x[lx1])
	#	xba$y <-c(xba$y,xb$y[lx1])
	reg <- invProj(xba$x, xba$y, geopar$scale, geopar$b0, geopar$b1, geopar$
		l1, geopar$projection)
	lxv[nholes + 2] <- length(xb$x)
	xb <- invProj(xb$x, xb$y, geopar$scale, geopar$b0, geopar$b1, geopar$
		l1, geopar$projection)
	if(geopar$projection == "none") {
		x <- xb$x
		y <- xb$y
		return(list(reg = reg, x = x, y = y, lxv = lxv))
	}
	else {
		lat <- xb$lat
		lon <- xb$lon
		return(list(reg = reg, lat = lat, lon = lon, lxv = lxv))
	}
}
fill.outside.border<-
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

selpos<-
function(lat, lon = NULL, ind)
{
	if(is.null(lon)) {
		lon <- lat$lon
		lat <- lat$lat
	}
	lat <- lat[ind]
	lon <- lon[ind]
	return(lat, lon)
}
geoarea<-
function(data, Projection = "Lambert", old.method = F, ngrdpts = 2000, robust
	 = T)
{
	area <- 0
	data <- geo.Split.poly(data)
	if(old.method) {
		for(i in 1:length(data))
			area <- area + geoarea.old(data[[i]], ngrdpts, robust)
	}
	else {
		area <- 0
		for(i in 1:length(data)) {
			if(Projection == "Lambert")
				data[[i]] <- lambert(data[[i]]$lat, data[[i]]$
					lon, mean(data[[i]]$lat), mean(data[[
					i]]$lon), mean(data[[i]]$lat))
			else data[[i]] <- mercator(data[[i]]$lat, data[[i]]$
					lon, b0 = mean(data[[i]]$lat))
			data[[i]] <- data.frame(x = data[[i]]$x, y = data[[
				i]]$y)
			n <- nrow(data[[i]])
			area <- area + abs(sum(data[[i]]$x[1:(n - 1)] * data[[
				i]]$y[2:n] - data[[i]]$x[2:n] * data[[i]]$
				y[1:(n - 1)], na.rm = T)/2)
		}
	}
	return(area)
}

geo.Split.poly<-
function(data)
{
	while(is.na(data[nrow(data), "lat"])) data <- data[ - nrow(data),  ]
	while(is.na(data[1, "lat"])) data <- data[-1,  ]
	n <- nrow(data)
	if(any(is.na(data$lat))) {
		tmp <- list()
		i <- 1:nrow(data)
		i <- i[is.na(data$lat)]
		i1 <- c(1, i + 1)
		i2 <- c(i - 1, nrow(data))
		for(i in 1:length(i1)) {
			tmp[[i]] <- data[i1[i]:i2[i],  ]
			n <- nrow(tmp[[i]])
			if(tmp[[i]]$lat[1] != tmp[[i]]$lat[n] || tmp[[i]]$
				lon[1] != tmp[[i]]$lon[n])
				tmp[[i]] <- rbind(tmp[[i]], tmp[[i]][1,  ])
			if(nrow(tmp[[i]]) < 4) {
				cat("minimum of 3 points needed to define area"
					)
				print(tmp[[i]][1:(nrow(tmp[[i]]) - 1)])
			}
		}
	}
	else {
		if(data$lat[1] != data$lat[n] || data$lon[1] != data$lon[n])
			data <- rbind(data, data[1,  ])
		tmp <- list(c1 = data)
	}
	return(tmp)
}

geocurve<-
function(data, df = nrow(data)/2, n = 10, open = T, arrow = "none", col = 1,
	lwd = 1, size = 0.2, angle = 15, smooth = T, plot = T, ...)
{
	if(df == "all")
		df <- nrow(data) - 1
	if(smooth) {
		if(open)
			data <- Open.curve(data, df, n)
		else {
			n <- nrow(data)
			if(data[1, "lat"] != data[n, "lat"] || data[1, "lon"] !=
				data[n, "lon"])
				data <- rbind(data, data[1,  ])
			data <- Closed.curve(data, df, n)
		}
	}
	if(plot) {
		geolines(data, lwd = lwd, col = col, ...)
		if(arrow == "start" || arrow == "both")
			geolines.with.arrows(data, start = T, col = col, size
				 = size, angle = angle)
		if(arrow == "end" || arrow == "both")
			geolines.with.arrows(data, start = F, col = col, size
				 = size, angle = angle)
	}
	return(invisible(data))
}

geolines.with.arrows<-
function(data, start = T, size = 0.2, ...)
{
	if(!is.data.frame(data))
		data <- data.frame(data)
	n <- nrow(data)
	if(start)
		i <- c(1:n)
	else i <- seq(n, 1, by = -1)
	tmpdata <- data[i,  ]
	limits <- invProj(geopar$limx, geopar$limy)
	plt.size <- geopar$gpar$pin
	dlon <- size/plt.size[1] * diff(limits$lon)
	dlat <- size/plt.size[2] * diff(limits$lat)
	theta <- seq(0, 2 * pi, by = 0.1)
	lat <- tmpdata[1, "lat"] + dlat * sin(theta)
	lon <- tmpdata[1, "lon"] + dlon * cos(theta)
	circle <- data.frame(lat = lat, lon = lon)
	xr <- findline(tmpdata, circle, plot = F)
	i <- is.na(xr$lat)
	i1 <- c(1:length(i))
	i1 <- i1[i]
	n <- min(i1) - 1
	pos <- list(lat = c(xr$lat[n], tmpdata$lat[1]), lon = c(xr$lon[n],
		tmpdata$lon[1]))
	pos <- Arrow(pos, ...)
	return(invisible(pos))
}

Open.curve<-
function(gogn, df = nrow(gogn)/2, n = 10)
{
	gogn$index <- c(1:nrow(gogn))
 	assign("df", df,pos=1)#, frame = 1) # Changed for R ver, pos added.
        df <- round(df)
	x <- glm(lat ~ ns(index, df = df), data = gogn)
	y <- glm(lon ~ ns(index, df = df), data = gogn)
	r <- range(gogn$index)
	pred.frame <- data.frame(index = seq(r[1], r[2], length = nrow(gogn) *
		n))
	pred.frame$lat <- predict(x, pred.frame)
	pred.frame$lon <- predict(y, pred.frame)
	pred.frame <- pred.frame[, c("lat", "lon")]
	return(pred.frame)
}
Closed.curve<-
function(gogn, df = round(nrow(gogn)/2), n = 10)
{
	gogn$index <- c(1:nrow(gogn))
	tmpper <- c(1, nrow(gogn))
	assign("tmpper", tmpper,pos=1)#, frame = 1) changed for R ver.
	assign("df", df, pos=1)#frame = 1)
	x <- glm(lat ~ ps(index, df = df, period = tmpper), data = gogn)
	y <- glm(lon ~ ps(index, df = df, period = tmpper), data = gogn)
	r <- range(gogn$index)
	pred.frame <- data.frame(index = seq(r[1], r[2], length = nrow(gogn) *
		n))
	pred.frame$lat <- predict(x, pred.frame)
	pred.frame$lon <- predict(y, pred.frame)
	pred.frame <- pred.frame[, c("lat", "lon")]
	return(pred.frame)
}


Arrow <-
function (pos, angle = 15, col = 2)
{
    pos <- Proj(pos)
    dx <- -diff(pos$x)
    dy <- -diff(pos$y)
    d <- sqrt(dy * dy + dx * dx)
    d1 <- d * tan((angle * pi)/180)
    p1y <- pos$y[1] + d1/d * dx
    p1x <- pos$x[1] - d1/d * dy
    p2y <- pos$y[1] - d1/d * dx
    p2x <- pos$x[1] + d1/d * dy
    d <- data.frame(y = c(pos$y[2], p1y, p2y, pos$y[2]), x = c(pos$x[2],
        p1x, p2x, pos$x[2]))
    d <- invProj(d)
    geopolygon(d,col=col)
    return(invisible(data.frame(lat = d$lat, lon = d$lon)))
}


geodefine<-
function(nholes = 0)
{
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	border <- giveborder(nholes = nholes)
	reg <- border$reg
	par(oldpar)
	if(geopar$projection == "none")
		reg <- list(x = reg$x, y = reg$y)
	else reg <- list(lat = reg$lat, lon = reg$lon)
	reg <- data.frame(reg)
	return(reg)
}

geogrid<-
function(lat, lon = 0, col = 1, type = "l", lwd = 0, lty = 0, pch = "+")
{
	oldpar <- selectedpar()
	#par(geopar$gpar)
	if(length(lon) == 1) {
		if(geopar$projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	if(geopar$projection == "Lambert")
		nx <- 10
	else nx <- 1
	if(geopar$projection != "none") {
		# degrees and minutes
		if(mean(lat, na.rm = T) > 1000) {
			lat <- geoconvert(lat)
			lon <-  - geoconvert(lon)
		}
	}
	if(type == "l") {
		#		lat <- c(lat,NA) ; lon <- c(lon,NA)
		llon <- length(lon)
		llat <- length(lat)
		latgr <- t(matrix(lat, llat, llon))
		longr <- matrix(lon, llon, llat)
		n1 <- rep(NA, nrow(latgr))
		n2 <- rep(NA, ncol(latgr) + 1)
		latgr <- cbind(latgr, n1)
		latgr <- rbind(latgr, n2)
		longr <- cbind(longr, n1)
		longr <- rbind(longr, n2)
		geolines(latgr, longr, col = col, nx = nx)
		# plot the lines.
		llon <- length(lon)
		llat <- length(lat)
		latgr <- matrix(lat, llat, llon)
		longr <- t(matrix(lon, llon, llat))
		n1 <- rep(NA, nrow(latgr))
		n2 <- rep(NA, ncol(latgr) + 1)
		latgr <- cbind(latgr, n1)
		latgr <- rbind(latgr, n2)
		longr <- cbind(longr, n1)
		longr <- rbind(longr, n2)
		geolines(latgr, longr, col = col, lwd = lwd, lty = lty, nx = nx
			)
	}
	else {
		llon <- length(lon)
		llat <- length(lat)
		latgr <- c(t(matrix(lat, llat, llon)))
		longr <- c(matrix(lon, llon, llat))
		geopoints(latgr, longr, pch = pch)
	}
	return(invisible())
	par(oldpar)
}
orthproj<-
function(pts, curve)
{
        pts1 <- lambert(pts$lat, pts$lon, 65, -18, 65)
        curve1 <- lambert(curve$lat, curve$lon, 65, -18, 65)
        pts$x <- pts1$x/1.852
        pts$y <- pts1$y/1.852
        curve$x <- curve1$x/1.852
        curve$y <- curve1$y/1.852
        pardist <- perdist <- rep(0, length(pts$lat))
        x <- .C("Curvedist",
                as.double(curve$x),
                as.double(curve$y),
                as.double(curve$dist),
                as.integer(length(curve$x)),
                as.double(pts$x),
                as.double(pts$y),
                as.double(perdist),
                as.double(pardist),
                as.integer(length(pts$y)))
        pardist <- x[[7]]
        perdist <- x[[8]]
        # Points inside curve get negative perdist.
        i <- geoinside(pts, reg = curve, option = 0, robust = F)
        perdist[i] <-  - perdist[i]
        return(list(pardist = pardist, perdist = perdist))
}
reitaplott<-
function(reitur, smareitur = NULL, z, levels = NULL, colors = NULL, density = 1,
	maxcol = 155, nlevels = 6, white = F, border = F, angle = 45, rotate = 
	0, digits = 0, cex = 0, label.location = 0, labels.only = F, col = 1,
	outside = T, mincol = NULL)
{
	oldpar <- selectedpar()
	par(geopar$gpar)
	on.exit(par(oldpar))
	brd <- list(x = c(geopar$limx[1], geopar$limx[2], geopar$limx[2], 
		geopar$limx[1], geopar$limx[1]), y = c(geopar$limy[1], geopar$
		limy[1], geopar$limy[2], geopar$limy[2], geopar$limy[1]))
	if(geopar$cont)
		par(plt = geopar$contlines)
	z <- z + 1e-08
	brd$x <- brd$x + 1e-06
	brd$y <- brd$y + 1e-07
	if(abs(reitur[1]) > 999 && is.null(smareitur)) {
		smareitur <- abs(reitur) - floor(abs(reitur)/10) * 10
		reitur <- sign(reitur) * floor(abs(reitur)/10)
	}
	or.angle <- angle
	dlat <- c(0.125, 0.125, -0.125, -0.125)
	dlon <- c(-0.25, 0.25, -0.25, 0.25)
	lat <- r2d(reitur)
	lon <- lat$lon
	lat <- lat$lat
	if(!is.null(smareitur)) {
		lat <- lat + dlat[smareitur]
		lon <- lon + dlon[smareitur]
		dlon <- c(-0.25, 0.25, 0.25, -0.25, -0.25)
		dlat <- c(-0.125, -0.125, 0.125, 0.125, -0.125)
	}
	else {
		dlon <- c(-0.5, 0.5, 0.5, -0.5, -0.5)
		dlat <- c(-0.25, -0.25, 0.25, 0.25, -0.25)
	}
	nlat <- length(lat)
	nlon <- length(lon)
	lat <- t(matrix(lat, nlat, 5))
	lon <- t(matrix(lon, nlon, 5))
	dlon <- matrix(dlon, 5, nlat)
	dlat <- matrix(dlat, 5, nlat)
	lat <- lat + dlat
	lon <- lon + dlon
	n.a <- rep(NA, ncol(lat))
	lat <- rbind(lat, n.a)
	lon <- rbind(lon, n.a)
	# 	Set levels and color.  	
	z <- z + 1e-07
	# because of zeroes.  
	if(is.null(levels)) {
		if(nlevels == 0)
			nlevels <- 10
		levels <- pretty(range(z, na.rm = T), nlevels)
		levels <- levels[2:(length(levels) - 1)]
	}
	ncont <- length(levels)
	if(is.null(colors)) {
		if(density > 0 && is.null(mincol))
			mincol <- 8
		if(density == 0 && is.null(mincol))
			mincol <- 2
		if(density > 0 && is.null(maxcol))
			maxcol <- 70
		if(density == 0 && is.null(maxcol))
			maxcol <- 155
		if(white) {
			# lowest values white.  
			colors <- c(1:(ncont))
			colors <- floor(mincol + ((colors - 1) * (maxcol - 
				mincol))/(length(colors) - 1))
			colors <- c(0, colors)
		}
		else {
			colors <- c(1:(ncont + 1))
			colors <- floor(mincol + ((colors - 1) * (maxcol - 
				mincol))/(length(colors) - 1))
		}
	}
	print(paste("calculated colors", paste(colors, collapse = ",")))
	print(paste("calculated levels", paste(levels, collapse = ",")))
	levels.1 <- levels
	colors.1 <- colors
	m <- max(z[!is.na(z)])
	if(!is.null(levels)) {
		i <- c(1:length(levels))
		i <- i[levels > max(z[!is.na(z)])]
		if(length(i) > 0) {
			levels <- levels[ - i]
			if(length(colors) > 1) {
				i <- i + 1
				colors <- colors[ - i]
			}
		}
	}
	levels <- c(levels, max(c(max(abs(levels)) * 1.1, max(z) * 1.1)))
	# change.
	levels <- c(min(c(min(z[z != -99999]) - 1, levels[1] - 1)), levels)
	if(levels[2] - levels[1] < 1)
		levels[1] <- levels[2] - 1
	print("adjusted contour are")
	print(round(levels, 3))
	print("adjusted colors are")
	print(round(colors, 3))
	#	Define color for each point.  
	ind <- cut(z, levels)
	ind1 <- ind
	ind <- colors[ind]
	# number of color. 
	x <- Proj(lat, lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
		geopar$projection)
	if(density > 0 || !outside) {
		rot <- rep(0, length(colors))
		rot[1] <- angle
		for(i in 2:length(colors))
			rot[i] <- rot[i - 1] + rotate
		rot <- rot[ind1]
		if(!labels.only) {
			for(i in 1:length(ind)) {
				if(geopar$projection == "Lambert")
					x1 <- fill.points(x$x[1:5, i], x$y[
						1:5, i], nx = 10, option = 2)
				else x1 <- list(x = x$x[1:5, i], y = x$y[1:
						5, i])
				if(!outside)
					x1 <- findcut(x1, brd)
				if(length(x1$x) > 3) {
					if(density > 0) {
						if(ind[i] > 100)
							polygon(x1$x, x1$y,
								col = 1, border
								 = border)
						else polygon(x1$x, x1$y, 
								density = ind[
								i], border = 
								border, angle
								 = rot[i])
					}
					else polygon(x1$x, x1$y, col = ind[
							i], border = border)
					if(border) {
						if(!outside)
							x1 <- findline(x1,
								brd)
						if(length(x1$x) != 0)
							lines(x1$x, x1$y, col
								 = col)
					}
				}
			}
		}
		#       Add  labels around plot
		if(geopar$cont) {
			par(plt = geopar$contlab)
			par(new = T)
			plot(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0), type = "l",
				axes = F, xlab = " ", ylab = " ")
			shading1(levels.1, digits, colors, angle = or.angle,
				rotate = rotate, cex = cex, fill = geopar$
				cont)
		}
	}
	else {
		# use colors 
		if(!labels.only) {
			if(geopar$projection == "Lambert")
				x <- fill.points(x, y, 10, option = 2)
			polygon(x$x, x$y, col = ind, border = border)
			if(border == T)
				lines(x$x, x$y)
		}
		#       Add  labels around plot
		if(geopar$cont) {
			par(plt = geopar$contlab)
			par(new = T)
			plot(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0), type = "l",
				axes = F, xlab = " ", ylab = " ")
			if(cex != 0)
				par(cex = cex)
			labels1(levels.1, digits, colors.1, fill = geopar$
				cont)
		}
	}
	# 	Add  labels around plot 
	if(length(label.location) == 1) if(label.location == "locator")
			label.location <- geolocator(n = 2)
	if(length(label.location) > 1) {
		#label located somewhere in drawing
		label.location <- Proj(label.location$lat, label.location$
			lon, geopar$scale, geopar$b0, geopar$b1, geopar$l1,
			geopar$projection)
		paint.window(label.location, border = T)
		if(density > 0)
			shading1(levels.1, digits, colors.1, angle = or.angle,
				rotate = rotate, cex = cex, xlim = 
				label.location$x, ylim = label.location$y)
		else {
			labels1(levels.1, digits, colors.1, xlim = 
				label.location$x, ylim = label.location$y)
		}
	}
	par(oldpar)
	return(invisible())
}
r2d<-
function(square)
{
	lat <- floor(square/100)
	lon <- (square - lat * 100) %% 50
	halfb <- (square - 100 * lat - lon)/100
	lon <-  - (lon + 0.5)
	lat <- lat + 60 + halfb + 0.25
	return(list(lat = lat, lon = lon))
}
ps <- 
function(x, df = NULL, knots = NULL, period = c(0., 2. * pi))
  {
        intercept <- T
        nx <- names(x)
        x <- as.vector(x)
        nax <- is.na(x)
        if(nas <- any(nax))
                x <- x[!nax]
        #       xrange <- range(x)
        sorder <- 4.
        xrange <- period
        if(!missing(df) && missing(knots)) {
                nknots <- df - sorder + (3. - intercept)
                if(nknots < 0.) {
                        nknots <- 0.
                        warning(paste("df was too small; have used ",
sorder -
                                (3. - intercept)))
                }
                if(nknots > 0.) {
                        knots <- seq(from = 0., to = 1., length =
nknots + 2.)[
                                 - c(1., nknots + 2.)]
                        knots <- quantile(x, knots)
                }
                else knots <- NULL
        }
        Aknots <- sort(c(rep(xrange, 4.), knots))
        derivs <- c(2., 2., 1., 1., integer(length(x)))
        x <- c(period, period, x)
        junk <- spline.des(Aknots, x, sorder, derivs)$design
        secondder <- junk[1.:2.,  ]
        firstder <- junk[3.:4.,  ]
        junk <- junk[,  - c(1., ncol(junk))]
        n <- ncol(junk)
        # Hlutfall 1 og 2 afleidu i 0
        derrat1 <- junk[1., 1.]/junk[1., 2.]
        # Hlutfall 1 og 2 afleidu i n
        derrat2 <- junk[2., n]/junk[2., n - 1.]
        # c1 og cn med adra afleidu 0.
        c1 <- junk[, 1.] - derrat1 * junk[, 2.]
        cn <- junk[, n] - derrat2 * junk[, n - 1.]
        ratio <- c1[3.]/cn[4.]
        # Fyrsti diffurkvoti
        ct <- c1 + ratio * cn
        ratio <- junk[1., 2.]/junk[2., n - 1.]
        ct1 <- junk[, 2.] + junk[, n - 1.] * ratio
        basis <- cbind(junk[,  - c(1., 2., n - 1., n)], ct, ct1)
        basis <- basis[ - (1.:4.),  ]
        basis
}

 variogram<-
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
		if(scale == "Miles") rad <- rad/1.852
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
		if(scale == "Miles")
			maxdist <- maxdist/1.852
	}
	varioa <- dista <- numbera <- rep(0, nbins)
	if(evennumber)
		nbins <- nbins * 10
	ddist <- maxdist/nbins
	dist <- vario <- number <- rep(0, nbins)
	dist <- .C("variogram",
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
pdistx<-
function(y, x, y1, x1)
{
	return(sqrt((x - x1)^2. + (y - y1)^2.))
}
pdist<-
function(lat, lon, lat1, lon1)
{
	rad <- 6367
	#radius of earth in km
	return(rad * acos(sin(lat) * sin(lat1) + cos(lat) * cos(lat1) * cos(
		lon - lon1)))
}
plvar<-
function(vagram, n = 4, fit = T, type = "p")
{
	if(fit) {
		vagr1 <- vagram$dist[vagram$dist < vagram$rang1 * n]
		# n x range
		zva <- vagram$vario[1:length(vagr1)]
		plot(vagr1, zva, xlim = c(0, max(vagr1)), ylim = c(0, max(
			zva) * 1.05), xlab = "Distance", ylab = "Variogram",
			title = " ", type = type)
		lines(c(0, vagr1[1]/2, vagr1), spherical(vagram$rang1, vagram$
			sill, vagram$nugget, c(0, vagr1[1]/2, vagr1)))
		tloc <- c(max(vagr1) * 0.9, max(zva) * 1.04)
		tloc <- matrix(tloc, 2, 2, byrow = T)
		tloc[2, 2] <- max(zva) * 1.01
		tmp <- vagram$nugget/vagram$sill
		tmp <- round(tmp, digits = 2)
		tmp <- as.character(tmp)
		tmp <- substring(tmp, 1, 4)
		txt <- c(paste("nugget/sill=", tmp), paste("range = ", 
			as.character(round(vagram$rang1, digits = 2))))
		print(txt)
		text(tloc, txt)
	}
	else {
		plot(c(0, vagram$dist), c(0, vagram$vario), ylim = c(0, max(
			vagram$vario)), xlab = "Distance", ylab = "Variogram",
			title = " ")
	}
}
s2pre<-
function(data, file = "splus.pre", na.replace = "")
{
	# data       :matrix or data.frame.
	# na.replace :a character to replace NA with.
	#
	# VALUE      :a prelude file, named "Splus.pre" by default.
	if(is.data.frame(data)) data <- as.matrix.data.frame(data)
	data[is.na(data) | data == "NA"] <- na.replace
	col.names <- dimnames(data)[[2]]
	if(is.null(col.names) || length(col.names) == 0)
		col.names <- paste("dalkur", 1:ncol(data), sep = "")
	row.names <- dimnames(data)[[1]]
	if(!is.null(row.names) && length(row.names) > 0) {
		col.names <- c("linu_nofn", col.names)
		data <- cbind(row.names, data)
	}
	n.of.col <- length(col.names)
	# Write out rownames:
	cat(col.names, sep = c(rep("\t", n.of.col - 1), "\n"), file = file)
	strika.lina <- rep("", n.of.col)
	for(i in 1:n.of.col)
		strika.lina[i] <- paste(rep("-", nchar(col.names[i])), collapse
			 = "")
	# Write out the ------ line:
	cat(strika.lina, sep = c(rep("\t", n.of.col - 1), "\n"), file = file,
		append = T)
	# Write out the data:
	cat(t(data), sep = c(rep("\t", n.of.col - 1), "\n"), file = file, 
		append = T)
	return(invisible(NULL))
}
variofit<-
function(vagram, model = 1, option = 2, interactivt = F, sill = 0)
{
	if(model == 1 && !interactivt && option < 5) {
		if(sill == 0 && length(vagram$variance) > 0)
			vagram$variance <- sill
		vgr <- fitspher.aut.1(vagram, option, sill)
		if(vgr$error == 1)
			return()
		return(list(rang1 = vgr$rang1, sill = vgr$sill, nugget = vgr$
			nugget, dist = vagram$dist, vario = vagram$vario, 
			number = vagram$number))
	}
	if(interactivt) {
		k <- 1
		ld <- floor(length(vagram$dist)/1.2)
		sill <- rang1 <- nugget <- rep(0, 10)
		ans <- "y"
		col <- 10
		plvar(vagram, fit = F)
		#		cat(" What type of model : , Gaussian, spherical ")
		while(ans == "y" || ans == "Y") {
			cat(" Give sill, range and nugget  in this order : \n")
			x <- locator(n = 3)
			sill[k] <- x$y[1]
			rang1[k] <- x$x[2]
			nugget[k] <- x$y[3]
			txt0 <- paste(" k = ", as.character(k))
			txt1 <- paste(" sill =", as.character(round(sill[k],
				digits = 2)))
			txt2 <- paste("range = ", as.character(round(rang1[
				k], digits = 2)))
			txt3 <- paste("nugget = ", as.character(round(nugget[
				k], digits = 2)))
			print(txt1)
			print(txt2)
			print(txt3)
			lines(vagram$dist, spherical(rang1[k], sill[k], nugget[
				k], vagram$dist), col = col)
			text(vagram$dist[ld], sill[k], as.character(k), col = 
				col)
			col <- col + 10
			cat(" \n Try again y/n  : ")
			ans <- readline()
			k <- k + 1
			if(k == 10)
				break()
		}
		nm <- 0
		cat("Give the number of the best model, default the last one:")
		nm <- scan(n = 1)
		if(length(nm) == 0)
			nm <- k - 1
		# default.  
		rang1 <- rang1[nm]
		sill <- sill[nm]
		nugget <- nugget[nm]
	}
	return(list(rang1 = rang1, sill = sill, nugget = nugget, dist = vagram$
		dist, vario = vagram$vario, number = vagram$number))
}

fitspher.aut.1<-
function(vagram, option, sill)
{
	i1 <- c(2:(length(vagram$vario) - 1))
	# index vector
	zvar1 <- supsmu(vagram$dist, vagram$vario)
	# Do the fitting, find range, sill and nugget effect.
	# Max value of supersmoother.  		
	if(option == 1) ind <- i1[zvar1$y[i1] == max(zvar1$y[i1])]
	# First maximum of supersmoother
	if(option == 2) ind <- i1[(zvar1$y[i1] > zvar1$y[i1 + 1]) & (zvar1$
			y[i1] > zvar1$y[i1 - 1])][1]
	# Second maximum of supersmoother.   
	if(option == 3) ind <- i1[(zvar1$y[i1] > zvar1$y[i1 + 1]) & (zvar1$
			y[i1] > zvar1$y[i1 - 1])][2]
	# Sill given find range
	if(option == 4) {
		if(sill == 0)
			sill <- vagram$variance
		ind <- i1[zvar1$y[i1] > sill][1]
	}
	if(is.na(ind) && (option != 1)) {
		print(" condition not satisfied.  Max value of")
		print(" supersmoother used ")
		ind <- i1[zvar1$y[i1] == max(zvar1$y[i1])]
		option == 1
	}
	if(length(ind) == 0 && (option == 1)) {
		print(" cannot fit variogram")
		error <- 1
		return(error)
	}
	rang1 <- zvar1$x[ind]
	# range
	if(option != 4) sill <- zvar1$y[ind]
	# sill
	xvar2 <- (1.5 * vagram$dist[1:ind])/rang1 - (0.5 * vagram$dist[1:ind]^
		3)/rang1^3
	zvar2 <- vagram$vario[1:ind] - sill * xvar2[1:ind]
	xvar2 <- 1 - xvar2
	nugget <- lsfit(xvar2, zvar2,  , F)$coef
	# 	if the nugget effect is estimated less than 0 it set to 0.05  
	if(nugget < 0) {
		nugget <- 0.05
	}
	error <- 0
	return(list(nugget = nugget, dist = dist, range = rang1, sill = sill,
		error = error))
}
supsmu<-
function(x, y, wt = rep(1, length(y)), span = "cv", periodic = F, bass = 0)
{
	if(span == "cv")
		span <- 0
	leny <- length(y)
	if(length(x) != leny)
		stop("number of observations in x and y must match.")
	if(length(wt) != leny)
		stop("number of weights must match number of observations.")
	if(span < 0 || span > 1)
		stop("span must be between 0 and 1.")
	if(periodic) {
		iper <- as.integer(2)
		xrange <- range(x)
		if(xrange[1] < 0 || xrange[2] > 1)
			stop("x must be between 0 and 1 for periodic smooth")
	}
	else iper <- as.integer(1)
	okay <- is.finite(x + y + wt)
	ord <- order(x[okay], y[okay])
	ord <- cumsum(!okay)[okay][ord] + ord
	leno <- length(ord)
	if(diff <- leny - leno)
		warning(paste(diff, 
			"observation(s) with NAs, NaNs and/or Infs deleted"))
	sc <- array(as.double(0), c(leno, 7))
	xsort <- as.double(x[ord])
	smoothed <- .Fortran("supsupsmu8",
		as.integer(leno),
		xsort,
		as.double(y[ord]),
		as.double(wt[ord]),
		iper,
		as.double(span),
		as.double(bass),
		double(leno),
		as.array(sc))[[8]]
	#eliminate duplicate xsort values and corresponding smoothed values
	dupx <- duplicated(xsort)
	list(x = xsort[!dupx], y = smoothed[!dupx])
}

geoworld<- function(regions = ".", exact = F, boundary = T, fill = F, color = 1, lwd = 1, 
	lty = 1, plot = T, type = "l", pch = ".", database = "world", 
	return.data = F,outside=F,allowed.size=80000)
{
  resolution <- 0 #1
  interior <- F
  r <- 1.2
  doproj <- F
 if(fill)
    interior <- T
  if(geopar$projection == "Lambert") {
                                        # complicated borders in lat,lon
    p1 <- list(x = c(geopar$limx[1], mean(geopar$limx), geopar$limx[
                 1], geopar$limx[2]), y = c(geopar$limy[1], geopar$limy[
                                        2], geopar$limy[2], geopar$limy[2]))
    limits <- invProj(p1$x, p1$y, geopar$scale, geopar$b0, geopar$
                      b1, geopar$l1, geopar$projection)
    xlim <- c(limits$lon[3], limits$lon[4])
    ylim <- c(limits$lat[1], limits$lat[2])
  }
  else {
    limits <- invProj(geopar$limx, geopar$limy, geopar$scale, 
                      geopar$b0, geopar$b1, geopar$l1, geopar$projection)
    xlim <- c(limits$lon[1], limits$lon[2])
    ylim <- c(limits$lat[1], limits$lat[2])
  }
  xlim <- mean(xlim) + r * (xlim - mean(xlim))	# add
  ylim <- mean(ylim) + r * (ylim - mean(ylim))	# to get everything
                                        # parameter checks
  coordtype <- maptype(database)	
                                        # turn the region names into a list of polygon numbers
  gon <- mapname(database, regions, exact)
  n <- length(gon)
  if(n == 0) stop("nothing to draw: no recognized region names")	
                                        # turn the polygon numbers into a list of polyline numbers
  line <- mapgetg(database, gon, fill, c(-1000000, 1000000), c(-1000000, 
                                                               1000000))
  if(length(line$number) == 0) stop(
             "nothing to draw: all regions out of bounds")	
                                        # turn the polyline numbers into x and y coordinates
  if(fill)
    coord <- mapgetl(database, line$number, c(-1000000, 1000000), c(
                                                                    -1000000, 1000000))
  else {
    l <- abs(line$number)
    if(boundary && interior)
      l <- unique(l)
    else if(boundary)
      l <- l[!match(l, l[duplicated(l)], F)]
    else l <- l[duplicated(l)]
    coord <- mapgetl(database, l, xlim, ylim)
    if(length(coord) == 0)
      stop("all data out of bounds")
  }
  if(doproj) {
    coord <- Proj(coord$y, coord$x, geopar$scale, geopar$b0, geopar$
                  b1, geopar$l1, geopar$projection)
    coord$error <- F
  }
                                        # for filled regions, turn NA breaks at polylines into
                                        # NA breaks at polygons, deleting polygons for which
                                        # there is a corresponding NA color
  if(fill) {
    gonsize <- line$size
    color <- rep(color, length = length(gonsize))
    keep <- !is.na(color)
    coord[c("x", "y")] <- makepoly(coord, gonsize, keep)
    color <- color[keep]
  }
  if(return.data) return(data.frame(lat = coord$y, lon = coord$x))	
                                        # do the plotting, if requested
  data <- data.frame(lat=coord$y, lon = coord$x)
  if(plot){
    if(fill) geopolygon(data,col=color,allowed.size=allowed.size)
    if(!fill) geolines(data,col=color,lwd=lwd,lty=lty)
  }
  return(invisible())
}

# Set up a map for the area around Iceland.  Eyjar means Islands.

SMB.std.background <- 
function(depth,depthcol=1,depthlty=1,depthlwd=1,eyjar,depthlab,depthlabcsi=0.12,...) {
  SMB.limits <- list(lat=c(62.85,67.5),lon=c(-27.8,-9.8))
  geoplot(xlim=SMB.limits,...)
  if(!missing(depth)) gbplot(depth,depthcol,depthlty,depthlwd,depthlab,depthlabcsi)
  if(!missing(eyjar)) geolines(eyjar,...)
}

"utm"<-
function(lat, lon = 0, lon0 = -21)
{
	if(!is.null(lat$lon)) {
		lon <- lat$lon
		lat <- lat$lat
	}
#	lengd
#	 storas sporbaugsflatar metrar
	a <- 6378388	# hayford
#	a <- 6378206.4   # clarkes
# 	slettun (flattening)
	f <- 1/297	# 	eccentrisitet
	e <- sqrt(2/297 - 1/297^2)	# 	lengdar i midju
#	skoelunarfaktor UTM
	k0 <- 0.99960000000000004	
	# 	utreikningar, formulur x og y bls 61 Mapproj.-Working man.
	rlon <- ( - lon * pi)/180
	rlat <- (lat * pi)/180
	e2 <- 0.0067686600000000001
	emerki2 <- e2/(1 - e2)
	N <- a/sqrt(1 - e^2 * (sin(rlat))^2)
	TT <- (tan(rlat))^2
	C <- emerki2 * (cos(rlat))^2
	A <- ( - ( - lon - ( - lon0)) * pi)/180 * cos(rlat)
	M0 <- 0
	M <- a * ((1 - e^2/4 - (3 * e^4)/64 - (5 * e^6)/256) * rlat - ((3 * e^2
		)/8 + (3 * e^4)/32 + (45 * e^6)/256) * sin(2 * rlat) + ((15 * e^
		4)/256 + (45 * e^6)/1024) * sin(4 * rlat) - ((35 * e^6)/3072) * 
		sin(6 * rlat))
	y <- k0 * N * ((A + ((1 - TT + C) * A^3)/6 + ((5 - 18 * TT + TT^2 + 72 * 
		C - 58 * emerki2) * A^5)/120)) + 500000
	x <- k0 * (M - M0 + N * tan(rlat) * (A^2/2 + ((5 - TT + 9 * C + 4 * C^2
		) * A^4)/24 + ((61 - 58 * TT + TT^2 + 600 * C - 330 * emerki2) * 
		A^6)/720))
	k <- k0 * (1 + ((1 + C) * A^2)/2 + ((5 - 4 * TT + 42 * C + 13 * C^2 - 
		28 * emerki2) * A^2)/24 + ((61 - 148 * TT + 16 * TT^2) * A^6)/
		720)
	return(list(lat = lat, lon = lon, x = x, y = y, projection = "utm", 
		lon0 = lon0))
}

# Only for the Icelandic area.

SMB.std.background <- 
function(depth, depthcol = 1, depthlty = 1, depthlwd = 1, eyjar, depthlab,
        depthlabcsi = 0.12, ...)
{
        SMB.limits <- list(lat = c(62.85, 67.5), lon = c(-27.8, -9.8))
        geoplot(xlim = SMB.limits, ...)
        if(!missing(depth))
                gbplot(depth, depthcol, depthlty, depthlwd, depthlab,
                        depthlabcsi)
        if(!missing(eyjar))
                geolines(eyjar, ...)
}

pltgrid <- 
function(xgrid=NULL, ygrid=NULL, xpos, ypos, ...)
{
        if(!is.null(xgrid)) {
                if(missing(xpos))
                        xpos <- seq(par()$xaxp[1.], par()$xaxp[2.], length =
                                par()$xaxp[3.] + 1.)
                ypos1 <- xpos
                xpos <- matrix(xpos, length(xpos), 3.)
                xpos[, 3.] <- rep(NA, nrow(xpos))
                ypos1 <- xpos
                ypos1[, 1.] <- par()$usr[3.]
                ypos1[, 2.] <- par()$usr[4.]
                lines(t(xpos), t(ypos1), ...)
        }
        if(!is.null(ygrid)) {
                if(missing(ypos))
                        ypos <- seq(par()$yaxp[1.], par()$yaxp[2.], length =
                                par()$yaxp[3.] + 1.)
                print(ypos)
                ypos <- matrix(ypos, length(ypos), 3.)
                ypos[, 3.] <- rep(NA, nrow(ypos))
                xpos <- ypos
                xpos[, 1.] <- par()$usr[2.]
                xpos[, 2.] <- par()$usr[1.]
                lines(t(xpos), t(ypos), ...)
        }
}

"labels.size" <- 
function(cont, digits, sizes, xlim = c(0, 1), ylim = c(0, 1), fill = F, n,
        rat, minsym = "<", label.resolution = 0, open = F, lwd = 1, col = 1)
{
        xlim <- sort(xlim)
        ylim <- sort(ylim)
        ncont <- length(cont)
        lbox <- ncont + 1
        if(fill)
                lbox <- max(lbox, 20)
        boxy <- c(1:lbox)
        boxy <-  - boxy/lbox + 1
        boxy1 <- boxy + 1/(1.2 * lbox)
        if(fill) {
                boxy <- boxy[1:(ncont + 1)]
                boxy1 <- boxy1[1:(ncont + 1)]
        }
        yloc <- (boxy + boxy1)/2
        xloc <- matrix(0.85, length(yloc))
        theta <- (c(0:n) * 2 * pi)/n
        theta <- c(theta, NA)
        theta <- c(matrix(theta, n + 2, length(yloc)))
        par(adj = 0)
        cont <- round(cont, digits = digits)
        textx <- c(1:(length(cont) - 1))
        textx1 <- textx
        textx <- format(round(cont[1:(length(cont) - 1)] + label.resolution,
                digits = digits))
        textx1 <- format(round(cont[2:length(cont)], digits = digits))
        textx <- paste(textx, "-", textx1)
        minsym <- paste(minsym, " ", sep = "")
        tmp1 <- paste(minsym, format(round(cont[1], digits = digits)))
        tmp2 <- paste("> ", format(round(cont[ncont], digits = digits)))
        textx <- c(tmp1, textx, tmp2)
        boxx <- c(matrix(0.1, 1, length(boxy)))
        boxx <- xlim[1] + abs((xlim[2] - xlim[1])) * boxx
        xloc <- xlim[1] + abs((xlim[2] - xlim[1])) * xloc
        yloc <- ylim[1] + abs((ylim[2] - ylim[1])) * yloc
        boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
        ll <- (ylim[2] - ylim[1]) * 0.05
        # put the labels.
        if(fill) text(boxx, boxy + ll/2, textx) else text(boxx, boxy + ll,
                        textx)
        # put the labels.
        theta <- (c(0:n) * 2 * pi)/n
        theta <- c(theta, NA)
        theta <- c(matrix(theta, n + 2, length(boxy)))
        y <- c(t(matrix(yloc, length(yloc), n + 2)))
        x <- c(t(matrix(xloc, length(xloc), n + 2)))
        sizes <- c(t(matrix(sizes, length(boxx), n + 2)))
        y <- y + sizes * rat * sin(theta)
        x <- x + sizes * rat * cos(theta)
        if(!open)
                polygon(x, y, col = col, border = T)
        else lines(x, y, col = col, lwd = lwd)
}

"locdist" <- 
function(scale = "Miles", type = "p")
{
        lat <- geolocator(n = 2, type = type)
        x <- arcdist(lat$lat[1], lat$lon[1], lat$lat[2], lat$lon[2])
        if(scale == "km")
                x <- x * 1.852
        return(x)
}

currentarrows <- function(data,maxsize=0.5,maxn,col="blue",lwd=2,minrat=0.5,arrowsize=0.2,center=T){
  res <- list()
  xsizerat <- geopar$gpar$pin[1]/diff(geopar$origin$lon)
  ysizerat <- geopar$gpar$pin[2]/diff(geopar$origin$lat)
  data$rat <- cos(data$lat*pi/180) 
  if(missing(maxn)) 
    maxn <- max(data$current)
  
  tmp <- data.frame(lat=c(1,1),lon=c(1,1)) 
  for(i in 1:nrow(data)) {
    tmp[1,] <- data[i,c("lat","lon")]
    tmp[2,"lon"] <- tmp[1,"lon"]+maxsize*data$current[i]/maxn*cos(data$angle[i]*pi/180)/data$rat[i]/ysizerat
    tmp[2,"lat"] <- tmp[1,"lat"]+maxsize*data$current[i]/maxn*sin(data$angle[i]*pi/180)/ysizerat
    if(center){ #center the arrow, else start
      dlat <- diff(tmp$lat)
      dlon <- diff(tmp$lon)
      tmp$lat <- tmp$lat -dlat/2
      tmp$lon <- tmp$lon -dlon/2
    }
    res[[i]] <- tmp
    SegmentWithArrow(tmp,lwd=lwd,size=arrowsize,col=col,minrat=minrat)
  }
  return(invisible(res)) 
}



SegmentWithArrow <- function(pos,angle=15,size=0.2,minrat=1,col="blue",lwd=2){
  plt.size <- geopar$gpar$pin
  dist <- arcdist(pos$lat[1],pos$lon[1],pos$lat[2],pos$lon[2],scale="Miles")
  arrowsize <- diff(geopar$origin$lat)*size/geopar$gpar$pin[2]*60
  rat <- min(c(arrowsize/dist,0.5))
  tmp <- pos
  tmp[1,] <- tmp[2,]+rat*(tmp[1,]-tmp[2,])
  tmp <- Proj(tmp)
  dx <- diff(tmp$x)
  dy <- diff(tmp$y) 
  rat <- tan(angle*pi/180)
  tmp1 <- data.frame(x=tmp$x[c(1,2,1)],y=tmp$y[c(1,2,1)])
  tmp1$y[1] <- tmp1$y[1]-rat*dx
  tmp1$x[1] <- tmp1$x[1]+rat*dy
  tmp1$y[3] <- tmp1$y[3]+rat*dx
  tmp1$x[3] <- tmp1$x[3]-rat*dy
  tmp1 <- invProj(tmp1)
  if(lwd > 0) geolines(pos,col=col,lwd=lwd)
  geopolygon(tmp1,col=col) 
}

