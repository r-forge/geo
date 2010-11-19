\name{ps}
\alias{ps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ps(x, df = NULL, knots = NULL, period = c(0, 2 * pi))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{df}{ ~~Describe \code{df} here~~ }
  \item{knots}{ ~~Describe \code{knots} here~~ }
  \item{period}{ ~~Describe \code{period} here~~ }
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
function(x, df = NULL, knots = NULL, period = c(0., 2. * pi))
  {
        intercept <- T
        nx <- names(x)
        x <- as.vector(x)
        nax <- is.na(x)
        if(nas <- any(nax))
                x <- x[!nax]
        #       xrange <- range(x)
        sorder <- 4.
        xrange <- period
        if(!missing(df) && missing(knots)) {
                nknots <- df - sorder + (3. - intercept)
                if(nknots < 0.) {
                        nknots <- 0.
                        warning(paste("df was too small; have used ",
sorder -
                                (3. - intercept)))
                }
                if(nknots > 0.) {
                        knots <- seq(from = 0., to = 1., length =
nknots + 2.)[
                                 - c(1., nknots + 2.)]
                        knots <- quantile(x, knots)
                }
                else knots <- NULL
        }
        Aknots <- sort(c(rep(xrange, 4.), knots))
        derivs <- c(2., 2., 1., 1., integer(length(x)))
        x <- c(period, period, x)
        junk <- spline.des(Aknots, x, sorder, derivs)$design
        secondder <- junk[1.:2.,  ]
        firstder <- junk[3.:4.,  ]
        junk <- junk[,  - c(1., ncol(junk))]
        n <- ncol(junk)
        # Hlutfall 1 og 2 afleidu i 0
        derrat1 <- junk[1., 1.]/junk[1., 2.]
        # Hlutfall 1 og 2 afleidu i n
        derrat2 <- junk[2., n]/junk[2., n - 1.]
        # c1 og cn med adra afleidu 0.
        c1 <- junk[, 1.] - derrat1 * junk[, 2.]
        cn <- junk[, n] - derrat2 * junk[, n - 1.]
        ratio <- c1[3.]/cn[4.]
        # Fyrsti diffurkvoti
        ct <- c1 + ratio * cn
        ratio <- junk[1., 2.]/junk[2., n - 1.]
        ct1 <- junk[, 2.] + junk[, n - 1.] * ratio
        basis <- cbind(junk[,  - c(1., 2., n - 1., n)], ct, ct1)
        basis <- basis[ - (1.:4.),  ]
        basis
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
