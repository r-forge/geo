\name{Arrow}
\alias{Arrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Arrow(pos, angle = 15, col = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos}{ ~~Describe \code{pos} here~~ }
  \item{angle}{ ~~Describe \code{angle} here~~ }
  \item{col}{ ~~Describe \code{col} here~~ }
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
function(pos, angle = 15, col = 2)
{
	pos <- Proj(pos)
	dx <-  - diff(pos$x)
	dy <-  - diff(pos$y)
	d <- sqrt(dy * dy + dx * dx)
	d1 <- d * tan((angle * pi)/180)
	p1y <- pos$y[1] + d1/d * dx
	p1x <- pos$x[1] - d1/d * dy
	p2y <- pos$y[1] - d1/d * dx
	p2x <- pos$x[1] + d1/d * dy
	d <- data.frame(y = c(pos$y[2], p1y, p2y, pos$y[2]), x = c(pos$x[2],
		p1x, p2x, pos$x[2]))
	oldpar <- selectedpar()
	on.exit(par(oldpar))
	par(geopar$gpar)
	polygon(d, col = col)
	d <- invProj(d)
	return(data.frame(lat = d$lat, lon = d$lon))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
