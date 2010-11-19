\name{litir}
\alias{litir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
litir(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ ~~Describe \code{n} here~~ }
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
function(n)
{
        x <- c(1:(n + 1))
         plot(x, x)
        y <- x
        for(j in 1:(n - 1)) {
                for(i in 1:(n + 1)) {
                        polygon(c(x[i], x[i + 1], x[i + 1], x[i]), c(y[j],
                                y[j], y[j + 1], y[j + 1]), col = ((j - 1) *
                                n + i - 1))
                        lines(c(x[i], x[i + 1], x[i + 1], x[i], x[i]), c(y[
                                j], y[j], y[j + 1], y[j + 1], y[j]), col = 1)
                        text((x[i] + x[i + 1])/2, (y[j] + y[j + 1])/2,
                                as.character((j - 1) * n + i - 1))
                }
        }
        return(invisible())
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
