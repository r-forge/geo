\name{geoaxis}
\alias{geoaxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
geoaxis(side, pos, dist, dlat = 0.5, dlon = 1, csi = 0.12, cex = 0.7, inside = T, r = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{ ~~Describe \code{side} here~~ }
  \item{pos}{ ~~Describe \code{pos} here~~ }
  \item{dist}{ ~~Describe \code{dist} here~~ }
  \item{dlat}{ ~~Describe \code{dlat} here~~ }
  \item{dlon}{ ~~Describe \code{dlon} here~~ }
  \item{csi}{ ~~Describe \code{csi} here~~ }
  \item{cex}{ ~~Describe \code{cex} here~~ }
  \item{inside}{ ~~Describe \code{inside} here~~ }
  \item{r}{ ~~Describe \code{r} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function(side, pos, dist, dlat = 0.5, dlon = 1,csi=0.12, cex = 0.7, inside = T, r = 1,
	...)
{
	m <- par()$cex * csi
	if(inside)
		m <-  - m
	if(side == 2 || side == 4)
		ratio <- diff(geopar$origin$lon)/geopar$gpar$pin[1]
	else ratio <- diff(geopar$origin$lat)/geopar$gpar$pin[2]
	if(side == 2 || side == 4) {
		if(missing(pos)) {
			pos1 <- (geopar$origin$lat[1] \%/\% dlat) * dlat - dlat
			pos2 <- (geopar$origin$lat[2] \%/\% dlat) * dlat + dlat
			pos <- seq(pos1, pos2, by = dlat)
		}
		pos <- pos[pos <= geopar$origin$lat[2] & pos >= geopar$origin$
			lat[1]]
		if(missing(dist)) {
			if((side == 4 && inside) || (side == 2 && !inside)) {
				lat1 <- pos \%\% 1
				lat2 <- lat1 * 60 \%\% 1
				if(any(lat2))
					lm <- 8
				else if(any(lat1))
					lm <- 6
				else lm <- 4
				dist <- lm * m * 0.6 * r
			}
			else if((side == 2 && inside) || (side == 4 && !inside)
				)
				dist <- m/2.5 * r
		}
	}
	else if(side == 3 || side == 1) {
		if(missing(dist))
			dist <- m * r
		if(missing(pos)) {
			pos1 <- (geopar$origin$lon[1] \%/\% dlon) * dlon - dlon
			pos2 <- (geopar$origin$lon[2] \%/\% dlon) * dlon + dlon
			pos <- seq(pos1, pos2, by = dlon)
		}
		pos <- pos[pos <= geopar$origin$lon[2] & pos >= geopar$origin$
			lon[1]]
	}
	if(side == 2 || side == 4) {
		if(side == 2)
			lonpos <- geopar$origin$lon[1] - ratio * dist
		if(side == 4)
			lonpos <- geopar$origin$lon[2] + ratio * dist
		lat1 <- trunc(pos)
		lat2 <- pos \%\% 1
		txt <- paste(lat1, "°", sep = "")
		lat2 <- round(lat2 * 60, 2)
		i <- lat2 > 0
		if(any(i))
			txt[i] <- paste(txt[i], lat2[i], "'", sep = "")
		geotext(pos, rep(lonpos, length(lat1)), adj = 0, txt, cex = cex,
			outside = T, ...)
	}
	else {
		if(side == 1)
			latpos <- geopar$origin$lat[1] - ratio * dist
		if(side == 3)
			latpos <- geopar$origin$lat[2] + ratio * dist
		pos1 <- abs(pos)
		lon1 <- trunc(pos1)
		lon2 <- pos1 \%\% 1
		txt <- paste(lon1, "°", sep = "")
		lon2 <- round(lon2 * 60, 2)
		i <- lon2 > 0
		if(any(i))
			txt[i] <- paste(txt[i], lon2[i], "'", sep = "")
		geotext(rep(latpos, length(pos)), pos, txt, adj = 0.5, cex = 
			cex, outside = T, ...)
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
