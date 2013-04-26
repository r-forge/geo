\name{labels_size}
\alias{labels_size}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
labels_size(cont, digits, sizes, xlim = c(0, 1), ylim = c(0, 1), fill = F, n, rat, minsym = "<", label.resolution = 0, open = F, lwd = 1, col = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cont}{ ~~Describe \code{cont} here~~ }
  \item{digits}{ ~~Describe \code{digits} here~~ }
  \item{sizes}{ ~~Describe \code{sizes} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{fill}{ ~~Describe \code{fill} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{rat}{ ~~Describe \code{rat} here~~ }
  \item{minsym}{ ~~Describe \code{minsym} here~~ }
  \item{label.resolution}{ ~~Describe \code{label.resolution} here~~ }
  \item{open}{ ~~Describe \code{open} here~~ }
  \item{lwd}{ ~~Describe \code{lwd} here~~ }
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

 ~Make other sections like Warning with section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(cont, digits, sizes, xlim = c(0, 1), ylim = c(0, 1), fill = F, n,
        rat, minsym = "<", label.resolution = 0, open = F, lwd = 1, col = 1)
{
        xlim <- sort(xlim)
        ylim <- sort(ylim)
        ncont <- length(cont)
        lbox <- ncont + 1
        if(fill)
                lbox <- max(lbox, 20)
        boxy <- c(1:lbox)
        boxy <-  - boxy/lbox + 1
        boxy1 <- boxy + 1/(1.2 * lbox)
        if(fill) {
                boxy <- boxy[1:(ncont + 1)]
                boxy1 <- boxy1[1:(ncont + 1)]
        }
        yloc <- (boxy + boxy1)/2
        xloc <- matrix(0.85, length(yloc))
        theta <- (c(0:n) * 2 * pi)/n
        theta <- c(theta, NA)
        theta <- c(matrix(theta, n + 2, length(yloc)))
        par(adj = 0)
        cont <- round(cont, digits = digits)
        textx <- c(1:(length(cont) - 1))
        textx1 <- textx
        textx <- format(round(cont[1:(length(cont) - 1)] + label.resolution,
                digits = digits))
        textx1 <- format(round(cont[2:length(cont)], digits = digits))
        textx <- paste(textx, "-", textx1)
        minsym <- paste(minsym, " ", sep = "")
        tmp1 <- paste(minsym, format(round(cont[1], digits = digits)))
        tmp2 <- paste("> ", format(round(cont[ncont], digits = digits)))
        textx <- c(tmp1, textx, tmp2)
        boxx <- c(matrix(0.1, 1, length(boxy)))
        boxx <- xlim[1] + abs((xlim[2] - xlim[1])) * boxx
        xloc <- xlim[1] + abs((xlim[2] - xlim[1])) * xloc
        yloc <- ylim[1] + abs((ylim[2] - ylim[1])) * yloc
        boxy <- ylim[1] + (ylim[2] - ylim[1]) * boxy
        ll <- (ylim[2] - ylim[1]) * 0.05
        # put the labels.
        if(fill) text(boxx, boxy + ll/2, textx) else text(boxx, boxy + ll,
                        textx)
        # put the labels.
        theta <- (c(0:n) * 2 * pi)/n
        theta <- c(theta, NA)
        theta <- c(matrix(theta, n + 2, length(boxy)))
        y <- c(t(matrix(yloc, length(yloc), n + 2)))
        x <- c(t(matrix(xloc, length(xloc), n + 2)))
        sizes <- c(t(matrix(sizes, length(boxx), n + 2)))
        y <- y + sizes * rat * sin(theta)
        x <- x + sizes * rat * cos(theta)
        if(!open)
                polygon(x, y, col = col, border = T)
        else lines(x, y, col = col, lwd = lwd)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
