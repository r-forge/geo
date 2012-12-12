\name{SMB.std.background}
\alias{SMB.std.background}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SMB.std.background(depth, depthcol = 1, depthlty = 1, depthlwd = 1, eyjar, depthlab, depthlabcsi = 0.12, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{depth}{ ~~Describe \code{depth} here~~ }
  \item{depthcol}{ ~~Describe \code{depthcol} here~~ }
  \item{depthlty}{ ~~Describe \code{depthlty} here~~ }
  \item{depthlwd}{ ~~Describe \code{depthlwd} here~~ }
  \item{eyjar}{ ~~Describe \code{eyjar} here~~ }
  \item{depthlab}{ ~~Describe \code{depthlab} here~~ }
  \item{depthlabcsi}{ ~~Describe \code{depthlabcsi} here~~ }
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
function(depth, depthcol = 1, depthlty = 1, depthlwd = 1, eyjar, depthlab,
        depthlabcsi = 0.12, ...)
{
        SMB.limits <- list(lat = c(62.85, 67.5), lon = c(-27.8, -9.8))
        geoplot(xlim = SMB.limits, ...)
        if(!missing(depth))
                gbplot(depth, depthcol, depthlty, depthlwd, depthlab,
                        depthlabcsi)
        if(!missing(eyjar))
                geolines(eyjar, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
