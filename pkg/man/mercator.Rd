\name{mercator}
\alias{mercator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mercator(lat, lon, scale = "km", b0 = 65)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{ ~~Describe \code{lat} here~~ }
  \item{lon}{ ~~Describe \code{lon} here~~ }
  \item{scale}{ ~~Describe \code{scale} here~~ }
  \item{b0}{ ~~Describe \code{b0} here~~ }
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
function(lat, lon, scale = "km", b0 = 65)
{
	radius <- 6378.388
	m.p.km <- 1.852
	mult <- radius
	if(scale != "km")
		mult <- mult/m.p.km
	l1 <- (lon * pi)/180
	b1 <- (lat * pi)/180
	b0 <- (b0 * pi)/180
	x <- mult * cos(b0) * l1
	y <- mult * cos(b0) * log((1 + sin(b1))/cos(b1))
	return(invisible(list(lat = lat, lon = lon, x = x, y = y, scale = scale,
		projection = "mercator", b0 = b0, L0 = NULL)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
