\name{adjust.grd}
\alias{adjust.grd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adjust.grd(ply, rat = 0.025)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ply}{ ~~Describe \code{ply} here~~ }
  \item{rat}{ ~~Describe \code{rat} here~~ }
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
function(ply, rat = 0.025)
{
	gx <- geopar$limx
	gy <- geopar$limy
	bx1 <- list(x = c(gx[1], gx[2], gx[2], gx[1], gx[1]), y = c(gy[1],
		gy[1], gy[2], gy[2], gy[1]))
	gx <- mean(gx) + (1 - rat) * (gx - mean(gx))
	gy <- mean(gy) + (1 - rat) * (gy - mean(gy))
	bx1 <- list(x = c(gx[1], gx[2], gx[2], gx[1], gx[1], bx1$x), y = c(
		gy[1], gy[1], gy[2], gy[2], gy[1], bx1$y))
	ply <- findline(ply, bx1)
	return(ply)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
