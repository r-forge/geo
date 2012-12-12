\name{fill.matrix}
\alias{fill.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fill.matrix(outcome, x, rownr, dalknr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcome}{ ~~Describe \code{outcome} here~~ }
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{rownr}{ ~~Describe \code{rownr} here~~ }
  \item{dalknr}{ ~~Describe \code{dalknr} here~~ }
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
function(outcome, x, rownr, dalknr)
{
        ind <- nrow(outcome) * (dalknr - 1) + rownr
        outcome[ind] <- x
        return(outcome)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
