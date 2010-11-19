\name{invmerc}
\alias{invmerc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
invmerc(x, y, scale = "km", b0 = 65)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
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

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, y, scale = "km", b0 = 65)
{
	radius <- 6378.388
	m.p.km <- 1.852
	mult <- radius
	if(scale != "km")
		mult <- mult/m.p.km
	b0 <- (b0 * pi)/180
	lon <- (x/(mult * cos(b0)) * 180)/pi
	# Have to find latitude by iteration.
	c1 <- exp(y/(mult * cos(b0)))
	lat1 <- c(1:length(y))
	lat1[1:length(y)] <- b0
	# initial guess
	lat <- c(1:length(y))
	ind <- c(1:length(y))
	#index.
	ind <- ind[c1 != "NA"]
	# NA dont work in sum.
	while(sum(abs(lat1[ind] - lat[ind]))/sum(abs(lat[ind])) > 1e-07) {
		lat <- lat1
		lat1 <- lat - ((1 + sin(lat))/cos(lat) - c1)/((1 + sin(lat))/
			(cos(lat)^2))
	}
	lat <- lat1
	lat <- (lat * 180)/pi
	return(invisible(list(lat = lat, lon = lon, x = x, y = y, scale = scale,
		projection = "mercator", b0 = b0, L0 = NULL)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
