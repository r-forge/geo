\name{geolines.with.arrows}
\alias{geolines.with.arrows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
geolines.with.arrows(data, start = T, size = 0.2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{start}{ ~~Describe \code{start} here~~ }
  \item{size}{ ~~Describe \code{size} here~~ }
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
function(data, start = T, size = 0.2, ...)
{
	if(!is.data.frame(data))
		data <- data.frame(data)
	n <- nrow(data)
	if(start)
		i <- c(1:n)
	else i <- seq(n, 1, by = -1)
	tmpdata <- data[i,  ]
	limits <- invProj(geopar$limx, geopar$limy)
	plt.size <- geopar$gpar$pin
	dlon <- size/plt.size[1] * diff(limits$lon)
	dlat <- size/plt.size[2] * diff(limits$lat)
	theta <- seq(0, 2 * pi, by = 0.1)
	lat <- tmpdata[1, "lat"] + dlat * sin(theta)
	lon <- tmpdata[1, "lon"] + dlon * cos(theta)
	circle <- data.frame(lat = lat, lon = lon)
	xr <- findline(tmpdata, circle, plot = F)
	i <- is.na(xr$lat)
	i1 <- c(1:length(i))
	i1 <- i1[i]
	n <- min(i1) - 1
	pos <- list(lat = c(xr$lat[n], tmpdata$lat[1]), lon = c(xr$lon[n],
		tmpdata$lon[1]))
	pos <- Arrow(pos, ...)
	return(invisible(pos))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
