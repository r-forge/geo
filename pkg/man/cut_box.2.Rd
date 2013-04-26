\name{cut_box.2}
\alias{cut_box.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cut_box.2(x, y, xb, yb)
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
	inds <- ind[is.na(x)]
	ind <- inds
	xx <- matrix(x,  , ind[1], byrow = T)
	yy <- matrix(y,  , ind[1], byrow = T)
	ind <- cut(x, xb,labels=FALSE) # labels=FALSE R ver.
	ind1 <- ind[2:length(ind)]
	ind <- ind[1:(length(ind) - 1)]
	ii <- c(1:length(ind))
	i <- ifelse(is.na(ind) & !is.na(ind1), ii, NA)
	i <- i[!is.na(i)]
	i1 <- ifelse(!is.na(ind) & is.na(ind1), ii, NA)
	i1 <- i1[!is.na(i1)]
	i2 <- c(1:length(ind))
	i2 <- i2[is.na(ind)]
	x2 <- y2 <- x3 <- y3 <- matrix(NA, nrow(xx), 3)
	x2[, 1] <- x[i]
	x2[, 2] <- x[i + 1]
	y2[, 1] <- y[i]
	y2[, 2] <- y[i + 1]
	x3[, 1] <- x[i1]
	x3[, 2] <- x[i1 + 1]
	y3[, 1] <- y[i1]
	y3[, 2] <- y[i1 + 1]
	t1 <- (xb[1] - x2[, 1])/(x2[, 2] - x2[, 1])
	t2 <- (xb[2] - x3[, 1])/(x3[, 2] - x3[, 1])
	x1 <- y1 <- matrix(NA, nrow(xx), 3)
	x1[, 1] <- x2[, 1] + t1 * (x2[, 2] - x2[, 1])
	x1[, 2] <- x3[, 1] + t2 * (x3[, 2] - x3[, 1])
	y1[, 1] <- y2[, 1] + t1 * (y2[, 2] - y2[, 1])
	y1[, 2] <- y3[, 1] + t2 * (y3[, 2] - y3[, 1])
	y[i2] <- NA
	x[i2] <- NA
	x[i] <- x1[, 1]
	y[i] <- y1[, 1]
	x[i1 + 1] <- x1[, 2]
	y[i1 + 1] <- y1[, 2]
	ind <- cut(y, c(-999999, yb, 999999),labels=FALSE) # labels=FALSE R ver.
	ind1 <- c(1:length(ind))
	ind1 <- ind1[ind != 2]
	x[ind1] <- NA
	y[ind1] <- NA
	return(list(x = x, y = y, x1 = c(x1[, 1]), y1 = c(y1[, 1])))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
