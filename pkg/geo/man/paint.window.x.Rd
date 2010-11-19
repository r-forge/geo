\name{paint.window.x}
\alias{paint.window.x}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
paint.window.x(listi, col = 0, border = T, poly = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{listi}{ ~~Describe \code{listi} here~~ }
  \item{col}{ ~~Describe \code{col} here~~ }
  \item{border}{ ~~Describe \code{border} here~~ }
  \item{poly}{ ~~Describe \code{poly} here~~ }
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
function(listi, col = 0., border = T, poly = T)
{
	x <- list(y = c(listi$y[1.], listi$y[1.], listi$y[2.], listi$y[2.],
		listi$y[1.]), x = c(listi$x[1.], listi$x[2.], listi$x[2.],
		listi$x[1.], listi$x[1.]))
	rx <- range(x$x)
	ry <- range(x$y)
	t1 <- c(rx[1.], rx[2.], rx[2.], rx[1.], rx[1.])
	t2 <- c(ry[1.], ry[1.], ry[2.], ry[2.], ry[1.])
	if(border) {
		mx <- mean(t1[1.:4.])
		my <- mean(t2[1.:4.])
		t11 <- t1 + 0.02 * (t1 - mx)
		t22 <- t2 + 0.02 * (t2 - my)
		lines(t11, t22, lwd = 1.5, col = 1.)
	}
	if(poly)
		polygon(t1, t2, col = 0.)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
