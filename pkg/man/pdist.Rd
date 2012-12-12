\name{pdist}
\alias{pdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pdist(lat, lon, lat1, lon1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{ ~~Describe \code{lat} here~~ }
  \item{lon}{ ~~Describe \code{lon} here~~ }
  \item{lat1}{ ~~Describe \code{lat1} here~~ }
  \item{lon1}{ ~~Describe \code{lon1} here~~ }
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
function(lat, lon, lat1, lon1)
{
	rad <- 6367
	#radius of earth in km
	return(rad * acos(sin(lat) * sin(lat1) + cos(lat) * cos(lat1) * cos(
		lon - lon1)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
