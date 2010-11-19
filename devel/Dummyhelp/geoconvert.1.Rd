\name{geoconvert.1}
\alias{geoconvert.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
geoconvert.1(x)
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

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x)
{
	i <- sign(x)
	x <- abs(x)
	# x <- ifelse(abs(x) < 10000, x * 100, x) # This can not be allowed.  
	# Check for minutes > 60
	x1 <- x \%\% 10000
	k <- c(1:length(x1))
	k <- k[x1 > 5999 & !is.na(x1)]
	if(length(k) > 0)
		print(paste("error > 60 min nr", k, x[k]))
	min <- (x/100) - trunc(x/10000) * 100
	return((i * (x + (200/3) * min))/10000)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
