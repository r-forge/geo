\name{extract}
\alias{extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract(grd, z, maxn = 10000, limits = NULL, col.names = c("lon", "lat"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grd}{ ~~Describe \code{grd} here~~ }
  \item{z}{ ~~Describe \code{z} here~~ }
  \item{maxn}{ ~~Describe \code{maxn} here~~ }
  \item{limits}{ ~~Describe \code{limits} here~~ }
  \item{col.names}{ ~~Describe \code{col.names} here~~ }
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
