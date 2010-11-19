\name{adapt}
\alias{adapt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adapt(reg.lat, reg.lon, projection = "Mercator")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reg.lat}{ ~~Describe \code{reg.lat} here~~ }
  \item{reg.lon}{ ~~Describe \code{reg.lon} here~~ }
  \item{projection}{ ~~Describe \code{projection} here~~ }
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
function(reg.lat, reg.lon, projection = "Mercator")
{
	ind <- c(1:length(reg.lat))
	nholes <- length(reg.lat[reg.lat == "NA"])
	lxv <- c(0:(nholes + 1))
	if(nholes != 0) {
		#               remove NA,s and points given twice
		ind1 <- ind[reg.lat == "NA"]
		ind2 <- c(ind1 - 1, ind1, length(reg.lat))
		lon <- reg.lon[ - ind2]
		lat <- reg.lat[ - ind2]
		for(i in 2:(nholes + 1)) {
			lxv[i] <- ind1[i - 1] - 2 * (i - 1)
		}
	}
	else {
		ind <- (1:length(reg.lon) - 1)
		lon <- reg.lon[ind]
		lat <- reg.lat[ind]
	}
	lxv[nholes + 2] <- length(lon)
	#x,y coordinates.   
	if(projection == "none") return(list(x = lon, y = lat, lxv = lxv))
		 else return(list(lat = lat, lon = lon, lxv = lxv))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
