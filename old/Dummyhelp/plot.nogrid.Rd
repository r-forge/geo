\name{plot.nogrid}
\alias{plot.nogrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.nogrid(o, xgr, ygr, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{o}{ ~~Describe \code{o} here~~ }
  \item{xgr}{ ~~Describe \code{xgr} here~~ }
  \item{ygr}{ ~~Describe \code{ygr} here~~ }
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
function(o, xgr, ygr, col)
{
	frame <- list(x = c(o$x[1], o$x[2], o$x[2], o$x[1], o$x[1]), y = c(
		o$y[1], o$y[1], o$y[2], o$y[2], o$y[1]))
	dx <- (o$x[2] - o$x[1])/100
	ly <- length(ygr)
	lx <- length(xgr)
	lengd <- ly * 2 + lx * 2
	o1 <- o$x[1]
	ind <- c(1:ly)
	my <- mx <- matrix(NA, lengd, 3)
	mx[ind, 1] <- o1
	mx[ind, 2] <- o1 + dx
	my[ind, 1] <- my[ind, 2] <- ygr
	o1 <- o$x[2]
	ind <- c((ly + 1):(ly * 2))
	mx[ind, 1] <- o1 - dx
	mx[ind, 2] <- o1
	my[ind, 1] <- my[ind, 2] <- ygr
	o1 <- o$y[1]
	ind <- c((ly * 2 + 1):(ly * 2 + lx))
	my[ind, 1] <- o1
	my[ind, 2] <- o1 + dx
	mx[ind, 1] <- mx[ind, 2] <- xgr
	o1 <- o$y[2]
	ind <- c((ly * 2 + lx + 1):(ly * 2 + lx * 2))
	my[ind, 1] <- o1 - dx
	my[ind, 2] <- o1
	mx[ind, 1] <- mx[ind, 2] <- xgr
	lines(t(mx), t(my), col = col)
	lines(frame, col = col)
	return(invisible())
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
