\name{prepare.line}
\alias{prepare.line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prepare.line(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
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
function(x)
{
	n <- length(x)
	x1 <- x[2:n]
	x2 <- x[1:(n - 1)]
	ind <- c(1:(n - 1))
	ind1 <- ind[!is.na(x1) & is.na(x2)]
	ind2 <- ind[is.na(x1) & !is.na(x2)]
	if(length(ind1) > 0)
		lx1 <- ind1 + 1
	if(length(ind2) > 0)
		lx2 <- ind2
	if(length(ind1) == 0)
		lx1 <- 1
	if(length(ind2) == 0)
		lx2 <- n
	if(!is.na(x[1]))
		lx1 <- unique(c(1, lx1))
	if(!is.na(x[n]))
		lx2 <- unique(c(lx2, n))
	nlx <- length(lx1)
	return(list(lx1 = lx1, lx2 = lx2, nlx = nlx))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
