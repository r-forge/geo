\name{plot.reitnr}
\alias{plot.reitnr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.reitnr(cexrt, lwd = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cexrt}{ ~~Describe \code{cexrt} here~~ }
  \item{lwd}{ ~~Describe \code{lwd} here~~ }
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
