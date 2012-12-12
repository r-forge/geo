\name{shading1}
\alias{shading1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
shading1(cont, digits, colors, xlim = c(0, 1), ylim = c(0, 1), fill = F, angle, rotate, cex, rat, minsym = "<")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cont}{ ~~Describe \code{cont} here~~ }
  \item{digits}{ ~~Describe \code{digits} here~~ }
  \item{colors}{ ~~Describe \code{colors} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{fill}{ ~~Describe \code{fill} here~~ }
  \item{angle}{ ~~Describe \code{angle} here~~ }
  \item{rotate}{ ~~Describe \code{rotate} here~~ }
  \item{cex}{ ~~Describe \code{cex} here~~ }
  \item{rat}{ ~~Describe \code{rat} here~~ }
  \item{minsym}{ ~~Describe \code{minsym} here~~ }
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
function(cont, digits, colors, xlim = c(0, 1), ylim = c(0, 1), fill = F, angle,
	rotate, cex, rat, minsym = "<")
{
	xlim <- sort(xlim)
	ylim <- sort(ylim)
	if(cex != 0)
		par(cex = cex)
	ncont <- length(cont)
	if(fill)
		lbox <- max(ncont + 1, 20)
	else lbox <- ncont + 1
	boxy <- c(1:lbox)
	boxy <-  - boxy/lbox + 1
	boxy1 <- boxy + 1/(1.2 * lbox)
	if(fill) {
		boxy <- boxy[1:(ncont + 1)]
		boxy1 <- boxy1[1:(ncont + 1)]
	}
	ymat <- matrix(0, 5, length(boxy))
	ymat[1,  ] <- boxy
	ymat[2,  ] <- boxy
	ymat[3,  ] <- boxy1
	ymat[4,  ] <- boxy1
	ymat[5,  ] <- NA
	xmat <- matrix(0, 5, length(boxy))
	xmat[1,  ] <- 0.75
	xmat[2,  ] <- 0.97
	xmat[3,  ] <- 0.97
	xmat[4,  ] <- 0.75
	xmat[5,  ] <- NA
	#       put  text in figure
	par(adj = 0)
	cont <- round(cont, digits = digits)
	textx <- c(1:(length(cont) - 1))
	textx1 <- textx
	textx <- as.character(round(cont[1:(length(cont) - 1)], digits = digits
		))
	textx1 <- as.character(round(cont[2:length(cont)], digits = digits))
	textx <- paste(textx, "-", textx1)
	minsym <- paste(minsym, " ", sep = "")
	textx <- c(paste(minsym, as.character(round(cont[1], digits = digits))),
		textx)
	textx[ncont + 1] <- paste("> ", as.character(round(cont[ncont], digits
		 = digits)))
	boxx <- c(matrix(0.1, 1, length(boxy)))
	boxx <- xlim[1] + (xlim[2] - xlim[1]) * boxx
	boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
	ll <- (ylim[2] - ylim[1]) * 0.05
	if(fill)
		text(boxx, boxy + ll/2, textx)
	else text(boxx, boxy + ll, textx)
	xmat <- xlim[1] + (xlim[2] - xlim[1]) * xmat
	ymat <- ylim[1] + (ylim[2] - ylim[1]) * ymat
	for(i in 1:length(colors)) {
		polygon(xmat[1:4, i], ymat[1:4, i], border = T, density = 
			colors[i], angle = angle)
		angle <- angle + rotate
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
