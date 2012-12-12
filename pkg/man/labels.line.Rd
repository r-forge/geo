\name{labels.line}
\alias{labels.line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
labels.line(cont, digits, colors, lty, xlim = c(0, 1), ylim = c(0, 1), linew = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cont}{ ~~Describe \code{cont} here~~ }
  \item{digits}{ ~~Describe \code{digits} here~~ }
  \item{colors}{ ~~Describe \code{colors} here~~ }
  \item{lty}{ ~~Describe \code{lty} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{linew}{ ~~Describe \code{linew} here~~ }
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
function(cont, digits, colors, lty, xlim = c(0, 1), ylim = c(0, 1), linew = F)
{
	xlim <- sort(xlim)
	ylim <- sort(ylim)
	ncont <- length(cont)
	if(length(lty) == ncont)
		linetypes <- T
	else linetypes <- F
	lbox <- ncont
	boxy <- c(1:lbox)
	boxy <-  - boxy/(lbox + 1) + 1
	boxy1 <- boxy + 1/(1.2 * lbox)
	ymat <- matrix(0, 2, length(boxy))
	ymat[1,  ] <- boxy
	ymat[2,  ] <- boxy
	xmat <- matrix(0, 2, length(boxy))
	xmat[1,  ] <- 0.7
	xmat[2,  ] <- 0.95
	#	put  text in figure
	par(adj = 0)
	cont <- round(cont, digits = digits)
	textx <- format(cont)
	boxx <- c(matrix(0.1, 1, length(boxy)))
	boxx <- xlim[1] + abs((xlim[2] - xlim[1])) * boxx
	boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
	ll <- (ylim[2] - ylim[1]) * 0.04
	text(boxx, boxy + ll, textx, col = 1)
	# put the lables.  
	xmat <- xlim[1] + abs((xlim[2] - xlim[1])) * xmat
	ymat <- ylim[1] + (ylim[2] - ylim[1]) * ymat
	for(i in 1:ncont) {
		if(linew)
			par(lwd = lwd[i])
		if(linetypes)
			par(lty = lty[i])
		lines(xmat[, i], ymat[, i] + ll, col = colors[i])
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
