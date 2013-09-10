geosymbols <-
function(lat, lon = 0, z, levels = NULL, reflevels = NULL, labels.only = FALSE,
	cex = 0.6, chs = 0.8, z1 = 0, circles = 0, squares = 0, rectangles = c(
	0, 0), vbars = 0, hbars = 0, perbars = 0, parbars = 0, sqrt = FALSE, col = 
	1, maxn = 0, colplot = FALSE, nlevels = 10, colors = 0, n = 25, maxcol = 
	155, only.positive = FALSE, digits = 0, white = FALSE, lwd = 1, label.location
	 = NULL, labels = 1, fill.circles = FALSE, density = 0, angle = 45, rotate
	 = 0, outside = FALSE, minsym = "<", boundcheck = 0, na.rm = TRUE, 
	label.resolution = 0, characters = FALSE, pch, marks, charcol = 0, 
	open.circles = FALSE, col.names = c("lat", "lon"), border = FALSE, bordercol = 
	0)
{
	geopar <- getOption("geopar")
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
		z[ind] <- mean(z, na.rm = TRUE)
	}
	if(fill.circles)
		colplot <- TRUE
	if(open.circles)
		colplot <- TRUE
	if(density > 0)
		colplot <- TRUE
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
		par(xpd = TRUE)
	else par(xpd = FALSE)
	if(colplot) {
		if(labels.only) {
			if(cex != 0)
				par(cex = cex)
			colsymbol(data$lat, data$lon, z, circles, squares,
				rectangles, hbars, vbars, perbars, parbars,
				levels, nlevels, colors, white, n, maxcol,
				digits, label.location, labels, fill.circles,
				density, angle, rotate, minsym, 
				label.resolution, col, labels.only = TRUE, 
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
						abs(z)/maxn), inches = FALSE, add
						 = TRUE, fg = col, lwd = lwd)
				if(!is.null(label.location))
					symbols(c(xloc), c(yloc), circles = 
						circles * sqrt(abs(levels)/
						maxn), add = TRUE, inches = FALSE,
						lwd = lwd, fg = col)
			}
			else {
				if(!labels.only)
					symbols(x, y, circles = circles * (
						abs(z)/maxn), add = TRUE, inches
						 = FALSE, fg = col, lwd = lwd)
				if(!is.null(label.location))
					symbols(c(xloc), c(yloc), circles = (
						circles * abs(levels))/maxn,
						add = TRUE, inches = FALSE, lwd = lwd, fg = col)
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
						abs(z)/maxn), add = TRUE, inches
						 = FALSE, fg = col, lwd = lwd)
				symbols(c(xloc), c(yloc), squares = squares *
					sqrt(abs(levels)/maxn), add = TRUE, inches
					 = FALSE, lwd = lwd, fg = col)
			}
			else {
				if(!labels.only)
					symbols(x, y, squares = squares * (
						abs(z)/maxn), add = TRUE, inches
						 = FALSE, fg = col, lwd = lwd)
				symbols(c(xloc), c(yloc), squares = (squares *
					abs(levels))/maxn, add = TRUE, inches = FALSE, fg = col,
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
			symbols(x, y, rectangles = m, add = TRUE, inches = FALSE,
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
			#      mlocx<- matrix(NA, 3, length(levels)); mlocy<-mlocx
			#      mlocx[1, ]<-c(xloc) ; mlocy[1, ]<-c(yloc); mlocx[2, ]<-c(xloc)
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
			#      mlocx<- matrix(NA, 3, length(levels)); mlocy<-mlocx
			#      mlocx[1, ]<-c(xloc) ; mlocy[1, ]<-c(yloc); mlocy[2, ]<-c(yloc)
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
			#      mlocx<- matrix(NA, 3, length(levels)); mlocy<-mlocx
			#      mlocx[1, ]<-c(xloc) ; mlocy[1, ]<-c(yloc); mlocy[2, ]<-c(yloc)
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
			#      if(sqrt)  mlocx[2, ]<-mlocx[1, ]+r*sqrt(abs(levels)/maxn)
			#      else  mlocx[2, ]<-mlocx[1, ]+r*abs(z)/maxn
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
			ind <- cut(z, levels, labels = FALSE)
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
			## if(is.numeric(levels))
			## 	Pointlabel(levels[2:(length(levels) - 1)],
			## 		digits, label.location$x, 
			## 		label.location$y, minsym, 
			## 		label.resolution, marks, pch, col,
			## 		cex, chs)
			## else Charlabel(levels, label.location$x, label.location$
			## 		y, label, marks, pch, col, cex, chs)
		}
	}
	options(warn = 0)
	return(invisible())
}

