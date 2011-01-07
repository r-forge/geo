\name{fill.points}
\alias{fill.points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fill.points(x, y, nx, option = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{nx}{ ~~Describe \code{nx} here~~ }
  \item{option}{ ~~Describe \code{option} here~~ }
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
function(x, y, nx, option = 1)
{
	n <- length(x)
	ny <- nx
	if(option != 1) {
		naind <- c(1:length(x))
		naind <- naind[is.na(x)]
	}
	dx <- (x[2:n] - x[1:(n - 1)])/(ny)
	dy <- (y[2:n] - y[1:(n - 1)])/(ny)
	x1 <- matrix(x[1:(n - 1)], n - 1, nx)
	y1 <- matrix(y[1:(n - 1)], n - 1, nx)
	ind <- c(0:(nx - 1))
	ind <- matrix(ind, n - 1, nx, byrow = T)
	dx <- matrix(dx, n - 1, nx)
	dy <- matrix(dy, n - 1, nx)
	x1 <- t(x1 + ind * dx)
	y1 <- t(y1 + ind * dy)
	ind <- c(1:length(y1))
	ind <- ind[is.na(y1) & row(y1) != 1]
	if(length(ind) != 0) {
		x1 <- x1[ - ind]
		y1 <- y1[ - ind]
	}
	if(is.na(x1[length(x1)])) {
		x1 <- c(x1, NA)
		y1 <- c(y1, NA)
	}
	ind <- c(1:length(x1))
	ind <- ind[is.na(x1)]
	if(length(ind) > 0) {
		ind <- matrix(ind,  , 2, byrow = T)
		if(option == 1) {
			ind <- ind[, 1]
			x1 <- x1[ - ind]
			y1 <- y1[ - ind]
		}
		else {
			ind <- ind[, 1]
			x1[ind] <- x[naind - 1]
			y1[ind] <- y[naind - 1]
		}
	}
	if(option != 1) {
		x1 <- c(x1, x[n])
		y1 <- c(y1, y[n])
	}
	return(list(x = x1, y = y1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
