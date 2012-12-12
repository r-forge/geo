\name{colsymbol}
\alias{colsymbol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colsymbol(lat, lon, z, circles, squares, rectangles, hbars, vbars, perbars, parbars, levels, nlevels, colors, white, n, maxcol, digits, label.location, labels, fill.circles, density, angle, rotate, minsym = "<", label.resolution = 0, col = 1, labels.only = F, open.circles, lwd, border = F, bordercol = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{ ~~Describe \code{lat} here~~ }
  \item{lon}{ ~~Describe \code{lon} here~~ }
  \item{z}{ ~~Describe \code{z} here~~ }
  \item{circles}{ ~~Describe \code{circles} here~~ }
  \item{squares}{ ~~Describe \code{squares} here~~ }
  \item{rectangles}{ ~~Describe \code{rectangles} here~~ }
  \item{hbars}{ ~~Describe \code{hbars} here~~ }
  \item{vbars}{ ~~Describe \code{vbars} here~~ }
  \item{perbars}{ ~~Describe \code{perbars} here~~ }
  \item{parbars}{ ~~Describe \code{parbars} here~~ }
  \item{levels}{ ~~Describe \code{levels} here~~ }
  \item{nlevels}{ ~~Describe \code{nlevels} here~~ }
  \item{colors}{ ~~Describe \code{colors} here~~ }
  \item{white}{ ~~Describe \code{white} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{maxcol}{ ~~Describe \code{maxcol} here~~ }
  \item{digits}{ ~~Describe \code{digits} here~~ }
  \item{label.location}{ ~~Describe \code{label.location} here~~ }
  \item{labels}{ ~~Describe \code{labels} here~~ }
  \item{fill.circles}{ ~~Describe \code{fill.circles} here~~ }
  \item{density}{ ~~Describe \code{density} here~~ }
  \item{angle}{ ~~Describe \code{angle} here~~ }
  \item{rotate}{ ~~Describe \code{rotate} here~~ }
  \item{minsym}{ ~~Describe \code{minsym} here~~ }
  \item{label.resolution}{ ~~Describe \code{label.resolution} here~~ }
  \item{col}{ ~~Describe \code{col} here~~ }
  \item{labels.only}{ ~~Describe \code{labels.only} here~~ }
  \item{open.circles}{ ~~Describe \code{open.circles} here~~ }
  \item{lwd}{ ~~Describe \code{lwd} here~~ }
  \item{border}{ ~~Describe \code{border} here~~ }
  \item{bordercol}{ ~~Describe \code{bordercol} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
