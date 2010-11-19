\name{setgrid}
\alias{setgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setgrid(lat, lon = 0, type = "p", pch = "*", xlim = c(0, 0), ylim = c(0, 0), b0 = 65, r = 1.1, country = island, xlab = "default", ylab = "default", option = "cut", reg = 0, dx = c(0, 0), nx = c(0, 0), grpkt = 0, scale = "km", find = F, new = F, grid = T, projection = "Mercator", n = 2500, b1 = b0, nholes = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{ ~~Describe \code{lat} here~~ }
  \item{lon}{ ~~Describe \code{lon} here~~ }
  \item{type}{ ~~Describe \code{type} here~~ }
  \item{pch}{ ~~Describe \code{pch} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{b0}{ ~~Describe \code{b0} here~~ }
  \item{r}{ ~~Describe \code{r} here~~ }
  \item{country}{ ~~Describe \code{country} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{option}{ ~~Describe \code{option} here~~ }
  \item{reg}{ ~~Describe \code{reg} here~~ }
  \item{dx}{ ~~Describe \code{dx} here~~ }
  \item{nx}{ ~~Describe \code{nx} here~~ }
  \item{grpkt}{ ~~Describe \code{grpkt} here~~ }
  \item{scale}{ ~~Describe \code{scale} here~~ }
  \item{find}{ ~~Describe \code{find} here~~ }
  \item{new}{ ~~Describe \code{new} here~~ }
  \item{grid}{ ~~Describe \code{grid} here~~ }
  \item{projection}{ ~~Describe \code{projection} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{b1}{ ~~Describe \code{b1} here~~ }
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

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
