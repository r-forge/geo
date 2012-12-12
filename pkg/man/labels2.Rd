\name{labels2}
\alias{labels2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
labels2(cont, digits, colors, xlim = c(0, 1), ylim = c(0, 1), nx = 4, fill = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cont}{ ~~Describe \code{cont} here~~ }
  \item{digits}{ ~~Describe \code{digits} here~~ }
  \item{colors}{ ~~Describe \code{colors} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{nx}{ ~~Describe \code{nx} here~~ }
  \item{fill}{ ~~Describe \code{fill} here~~ }
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
function(cont, digits, colors, xlim = c(0, 1), ylim = c(0, 1), nx = 4, fill = F
	)
{
	xlim <- sort(xlim)
	ylim <- sort(ylim)
	ncont <- length(cont)
	lbox <- ncont + 1
	if(fill)
		lbox <- max(lbox, 20)
	boxy <- c(1:lbox)
	boxy <-  - boxy/(lbox + 2) + 1
	dy <- 1/lbox
	boxy <- boxy - dy/2
	boxy1 <- boxy + 1/lbox
	ymat <- matrix(0, 5, length(boxy))
	ymat[1,  ] <- boxy
	ymat[2,  ] <- boxy
	ymat[3,  ] <- boxy1
	ymat[4,  ] <- boxy1
	ymat[5,  ] <- NA
	xmat <- matrix(0, 5, length(boxy))
	xmat[1,  ] <- 0.6
	xmat[2,  ] <- 0.9
	xmat[3,  ] <- 0.9
	xmat[4,  ] <- 0.6
	xmat[5,  ] <- NA
	#	put  text in figure
	ind <- c(1, c(1:floor((length(cont))/nx)) * nx)
	if(ind[length(ind)] == (length(cont)))
		ind <- c(ind, (length(cont)))
	par(adj = 0)
	cont <- round(cont, digits = digits)
	textx <- format(round(cont[ind], digits = digits))
	boxx <- c(matrix(0.1, 1, length(boxy)))
	boxx <- xlim[1] + (xlim[2] - xlim[1]) * boxx
	boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
	text(boxx[ind], boxy[ind], textx)
	# put the lables.  
	xmat <- xlim[1] + abs((xlim[2] - xlim[1])) * xmat
	ymat <- ylim[1] + (ylim[2] - ylim[1]) * ymat
	polygon(xmat, ymat, border = F, col = colors)
	if(colors[1] == 0) {
		xmat <- c(xmat[1:4], xmat[1])
		# if white color.  
		ymat <- c(ymat[1:4], ymat[1])
		lines(xmat, ymat)
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
