\name{cut_box.1}
\alias{cut_box.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cut_box.1(x, y, xb, yb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{xb}{ ~~Describe \code{xb} here~~ }
  \item{yb}{ ~~Describe \code{yb} here~~ }
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
function(x, y, xb, yb)
{
	ind <- c(1:length(x))
	ind <- ind[is.na(x)]
	x <- matrix(x,  , ind[1], byrow = T)
	y <- matrix(y,  , ind[1], byrow = T)
	n <- ind[1] - 1
	t1 <- (yb[1] - y[, 1])/(y[, n] - y[, 1])
	t2 <- (yb[2] - y[, 1])/(y[, n] - y[, 1])
	x1 <- y1 <- matrix(NA, nrow(x), 3)
	x1[, 1] <- x[, 1] + t1 * (x[, n] - x[, 1])
	x1[, 2] <- x[, 1] + t2 * (x[, n] - x[, 1])
	y1[, 1] <- y[, 1] + t1 * (y[, n] - y[, 1])
	y1[, 2] <- y[, 1] + t2 * (y[, n] - y[, 1])
	ind2 <- cut(x1[, 1], xb,labels=FALSE) # labels=FALSE R ver.
	ind <- c(1:length(ind2))
	ind2 <- ind[!is.na(ind2)]
	ind <- cut(x1[, 1], c(-9999999, xb),labels=FALSE) # labels=FALSE R ver.
	ind1 <- c(1:length(ind))
	ind1 <- ind1[!is.na(ind) & ind == 1]
	t1 <- (xb[1] - x[ind1, 1])/(x[ind1, n] - x[ind1, 1])
	x1[ind1, 1] <- x[ind1, 1] + t1 * (x[ind1, n] - x[ind1, 1])
	y1[ind1, 1] <- y[ind1, 1] + t1 * (y[ind1, n] - y[ind1, 1])
	ind1 <- c(1:length(ind))
	ind1 <- ind1[is.na(ind)]
	t1 <- (xb[2] - x[ind1, 1])/(x[ind1, n] - x[ind1, 1])
	x1[ind1, 1] <- x[ind1, 1] + t1 * (x[ind1, n] - x[ind1, 1])
	y1[ind1, 1] <- y[ind1, 1] + t1 * (y[ind1, n] - y[ind1, 1])
	ind <- cut(x1[, 2], c(-9999999, xb),labels=FALSE )# labels=FALSE R ver.
	ind1 <- c(1:length(ind))
	ind1 <- ind1[ind == 1 | is.na(ind)]
	x1[ind1,  ] <- NA
	y1[ind1,  ] <- NA
	return(list(x = t(x1), y = t(y1), ind = ind2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
