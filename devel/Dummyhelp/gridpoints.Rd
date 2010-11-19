\name{gridpoints}
\alias{gridpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gridpoints(border, dx, grpkt, nx, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{border}{ ~~Describe \code{border} here~~ }
  \item{dx}{ ~~Describe \code{dx} here~~ }
  \item{grpkt}{ ~~Describe \code{grpkt} here~~ }
  \item{nx}{ ~~Describe \code{nx} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
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

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
