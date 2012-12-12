\name{findline}
\alias{findline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findline(x, xb, plot = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{xb}{ ~~Describe \code{xb} here~~ }
  \item{plot}{ ~~Describe \code{plot} here~~ }
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
function(x, xb, plot = T)
{
	if(!plot) {
		x <- Proj(x)
		xb <- Proj(xb)
	}
	xr <- yr <- rep(0, 3 * (length(x$y) + length(xb$y)))
	nxr <- 0
	ab <- ab1 <- rep(0, length(xb$x))
	li <- prepare.line(x$x)
	ind <- c(1:length(x$x))
	ind <- ind[is.na(x$x)]
	if(length(ind) > 0)
		x$x[ind] <- x$y[ind] <- -999999
	xr <- .C("define_multiline",
		as.double(x$x),
		as.double(x$y),
		as.double(xb$x),
		as.double(xb$y),
		as.double(xr),
		as.double(yr),
		as.integer(length(x$y)),
		as.integer(length(xb$y)),
		as.integer(nxr),
		as.integer(li$lx1),
		as.integer(li$lx2),
		as.integer(li$nlx),
		as.integer(plot),
		as.double(ab),
		as.double(ab1))
	nxr <- xr[[9]]
	yr <- xr[[6]]
	yr <- yr[1:nxr]
	xr <- xr[[5]]
	xr <- xr[1:nxr]
	ind <- c(1:nxr)
	ind <- ind[xr < -999998]
	xr[ind] <- NA
	yr[ind] <- NA
	if(!plot) {
		xr <- invProj(xr, yr)
		xr <- data.frame(list(lat = xr$lat, lon = xr$lon))
		return(invisible(xr))
	}
	else return(list(y = yr, x = xr, nxr = nxr))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
