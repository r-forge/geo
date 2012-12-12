\name{pltgrid}
\alias{pltgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pltgrid(xgrid = NULL, ygrid = NULL, xpos, ypos, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xgrid}{ ~~Describe \code{xgrid} here~~ }
  \item{ygrid}{ ~~Describe \code{ygrid} here~~ }
  \item{xpos}{ ~~Describe \code{xpos} here~~ }
  \item{ypos}{ ~~Describe \code{ypos} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function(xgrid=NULL, ygrid=NULL, xpos, ypos, ...)
{
        if(!is.null(xgrid)) {
                if(missing(xpos))
                        xpos <- seq(par()$xaxp[1.], par()$xaxp[2.], length =
                                par()$xaxp[3.] + 1.)
                ypos <- xpos
                xpos <- matrix(xpos, length(xpos), 3.)
                xpos[, 3.] <- rep(NA, nrow(xpos))
                ypos1 <- xpos
                ypos1[, 1.] <- par()$usr[3.]
                ypos1[, 2.] <- par()$usr[4.]
                lines(t(xpos), t(ypos1), ...)
        }
        if(!is.null(ygrid)) {
                if(missing(ypos))
                        ypos <- seq(par()$yaxp[1.], par()$yaxp[2.], length =
                                par()$yaxp[3.] + 1.)
                print(ypos)
                ypos <- matrix(ypos, length(ypos), 3.)
                ypos[, 3.] <- rep(NA, nrow(ypos))
                xpos <- ypos
                xpos[, 1.] <- par()$usr[2.]
                xpos[, 2.] <- par()$usr[1.]
                lines(t(xpos), t(ypos), ...)
        }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
