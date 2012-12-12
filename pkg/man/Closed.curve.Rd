\name{Closed.curve}
\alias{Closed.curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Closed.curve(gogn, df = round(nrow(gogn)/2), n = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gogn}{ ~~Describe \code{gogn} here~~ }
  \item{df}{ ~~Describe \code{df} here~~ }
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

 ~Make other sections like Warning with section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(gogn, df = round(nrow(gogn)/2), n = 10)
{
	gogn$index <- c(1:nrow(gogn))
	tmpper <- c(1, nrow(gogn))
	assign("tmpper", tmpper,pos=1)#, frame = 1) changed for R ver.
	assign("df", df, pos=1)#frame = 1)
	x <- glm(lat ~ ps(index, df = df, period = tmpper), data = gogn)
	y <- glm(lon ~ ps(index, df = df, period = tmpper), data = gogn)
	r <- range(gogn$index)
	pred.frame <- data.frame(index = seq(r[1], r[2], length = nrow(gogn) *
		n))
	pred.frame$lat <- predict(x, pred.frame)
	pred.frame$lon <- predict(y, pred.frame)
	pred.frame <- pred.frame[, c("lat", "lon")]
	return(pred.frame)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
