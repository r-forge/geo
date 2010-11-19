geocontour <-
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

