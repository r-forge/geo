init <-
function(lat, lon = 0, type = "p", pch = "*", xlim = c(0, 0), ylim = c(0, 0),
	b0 = 65, r = 1.05, country = island, xlab = "Longitude", ylab = 
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

