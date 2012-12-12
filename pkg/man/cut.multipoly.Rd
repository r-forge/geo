\name{cut.multipoly}
\alias{cut.multipoly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cut.multipoly(x, xb, in.or.out = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{xb}{ ~~Describe \code{xb} here~~ }
  \item{in.or.out}{ ~~Describe \code{in.or.out} here~~ }
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
function(x, xb, in.or.out = 0)
{
	ind <- x$x[is.na(x$x)]
	if(length(ind) == 0) {
		x2 <- findcut(x, xb, in.or.out)
	}
	else {
		x2 <- list(x = NA, y = NA)
		ind <- prepare.line(x$x)
		for(i in 1:ind$nlx) {
			x1 <- list(x = x$x[ind$lx1[i]:ind$lx2[i]], y = x$y[
				ind$lx1[i]:ind$lx2[i]])
			x1 <- findcut(x1, xb, in.or.out)
			x2$x <- c(x2$x, NA, x1$x)
			x2$y <- c(x2$y, NA, x1$y)
		}
		x2$x <- x2$x[ - c(1:2)]
		x2$y <- x2$y[ - c(1:2)]
	}
	return(x2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
