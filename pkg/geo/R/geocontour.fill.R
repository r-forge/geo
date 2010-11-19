geocontour.fill <-
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

