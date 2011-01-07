\name{find.hnit}
\alias{find.hnit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find.hnit(pt, poly)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pt}{ ~~Describe \code{pt} here~~ }
  \item{poly}{ ~~Describe \code{poly} here~~ }
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
function(pt, poly)
{
	pt1 <- floor(pt)
	pt2 <- pt - pt1
	y <- poly$y[pt1] + pt2 * (poly$y[pt1 + 1] - poly$y[pt1])
	x <- poly$x[pt1] + pt2 * (poly$x[pt1 + 1] - poly$x[pt1])
	return(data.frame(x = x, y = y))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
