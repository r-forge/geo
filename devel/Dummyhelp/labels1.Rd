\name{labels1}
\alias{labels1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
labels1(cont, digits, colors, xlim = c(0, 1), ylim = c(0, 1), fill = F, minsym = "<", label.resolution = 0, labtxt = NULL, first.color.trans = T, mai = c(0, 1, 0, 1), leftrat = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cont}{ ~~Describe \code{cont} here~~ }
  \item{digits}{ ~~Describe \code{digits} here~~ }
  \item{colors}{ ~~Describe \code{colors} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{fill}{ ~~Describe \code{fill} here~~ }
  \item{minsym}{ ~~Describe \code{minsym} here~~ }
  \item{label.resolution}{ ~~Describe \code{label.resolution} here~~ }
  \item{labtxt}{ ~~Describe \code{labtxt} here~~ }
  \item{first.color.trans}{ ~~Describe \code{first.color.trans} here~~ }
  \item{mai}{ ~~Describe \code{mai} here~~ }
  \item{leftrat}{ ~~Describe \code{leftrat} here~~ }
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
function(cont, digits, colors, xlim = c(0, 1), ylim = c(0, 1), fill = F, minsym
	 = "<", label.resolution = 0, labtxt = NULL, first.color.trans = T,
	mai = c(0, 1, 0, 1), leftrat = 0.1)
{
	xlim <- sort(xlim)
	ylim <- sort(ylim)
	dx <- (xlim[2] - xlim[1])
	dy <- (ylim[2] - ylim[1])
	xlim[2] <- xlim[1] + mai[2] * dx
	xlim[1] <- xlim[1] + mai[1] * dx
	ylim[2] <- ylim[1] + mai[4] * dy
	ylim[1] <- ylim[1] + mai[3] * dy
	ncont <- length(cont)
	if(label.resolution == "none")
		lbox <- ncont
	else lbox <- ncont + 1
	if(fill)
		lbox <- max(lbox, 20)
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
	xmat[1,  ] <- 0.7
	xmat[2,  ] <- 0.95
	xmat[3,  ] <- 0.95
	xmat[4,  ] <- 0.7
	xmat[5,  ] <- NA
	#	put  text in figure
	par(adj = 0)
	cont <- round(cont, digits = digits)
	if(!(label.resolution == "none")) {
		textx <- c(1:(length(cont) - 1))
		textx1 <- textx
		textx <- format(round(cont[1:(length(cont) - 1)] + 
			label.resolution, digits = digits))
		textx1 <- format(round(cont[2:length(cont)], digits = digits))
		textx <- paste(textx, "-", textx1)
		tmp1 <- paste(minsym, format(round(cont[1], digits = digits)))
		tmp2 <- paste(">", format(round(cont[ncont], digits = digits)))
		textx <- c(tmp1, textx, tmp2)
	}
	else {
		print(cont)
		textx <- c(1:length(cont))
		testx <- format(round(cont), digits = digits)
	}
	print(1)
	boxx <- c(matrix(leftrat, 1, length(boxy)))
	boxx <- xlim[1] + abs((xlim[2] - xlim[1])) * boxx
	boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
	ll <- (ylim[2] - ylim[1]) * 0.05
	if(!is.null(labtxt))
		textx <- labtxt
	# put the labels. 
	if(fill) text(boxx, boxy + ll/2, textx) else text(boxx, boxy + ll,
			textx)
	# put the labels. 
	xmat <- xlim[1] + abs((xlim[2] - xlim[1])) * xmat
	ymat <- ylim[1] + (ylim[2] - ylim[1]) * ymat
	if(label.resolution == "none") {
		colors <- colors[2:length(colors)]
	}
	polygon(xmat, ymat, border = T, col = colors)
	if(colors[1] == 0 || first.color.trans) {
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
