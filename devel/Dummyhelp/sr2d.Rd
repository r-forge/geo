\name{sr2d}
\alias{sr2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sr2d(ssquare)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ssquare}{ ~~Describe \code{ssquare} here~~ }
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
function(ssquare)
{
        square <- floor(ssquare/10)
        ssquare <- ssquare - square * 10
        lat <- floor(square/100)
        lon <- (square - lat * 100) \%\% 50
        halfb <- (square - 100 * lat - lon)/100
        lon <-  - (lon + 0.5)
        lat <- lat + 60 + halfb + 0.25
        l1.lat <- c(0, 0.125, 0.125, -0.125, -0.125)
        l1.lon <- c(0, -0.25, 0.25, -0.25, 0.25)
        lat <- lat + l1.lat[ssquare + 1]
        lon <- lon + l1.lon[ssquare + 1]
        return(list(lat = lat, lon = lon))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
