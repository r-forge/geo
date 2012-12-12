\name{giveborder}
\alias{giveborder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
giveborder(nholes = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nholes}{ ~~Describe \code{nholes} here~~ }
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
