\name{fitspher.aut.1}
\alias{fitspher.aut.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fitspher.aut.1(vagram, option, sill)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vagram}{ ~~Describe \code{vagram} here~~ }
  \item{option}{ ~~Describe \code{option} here~~ }
  \item{sill}{ ~~Describe \code{sill} here~~ }
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
function(vagram, option, sill)
{
	i1 <- c(2:(length(vagram$vario) - 1))
	# index vector
	zvar1 <- supsmu(vagram$dist, vagram$vario)
	# Do the fitting, find range, sill and nugget effect.
	# Max value of supersmoother.  		
	if(option == 1) ind <- i1[zvar1$y[i1] == max(zvar1$y[i1])]
	# First maximum of supersmoother
	if(option == 2) ind <- i1[(zvar1$y[i1] > zvar1$y[i1 + 1]) & (zvar1$
			y[i1] > zvar1$y[i1 - 1])][1]
	# Second maximum of supersmoother.   
	if(option == 3) ind <- i1[(zvar1$y[i1] > zvar1$y[i1 + 1]) & (zvar1$
			y[i1] > zvar1$y[i1 - 1])][2]
	# Sill given find range
	if(option == 4) {
		if(sill == 0)
			sill <- vagram$variance
		ind <- i1[zvar1$y[i1] > sill][1]
	}
	if(is.na(ind) && (option != 1)) {
		print(" condition not satisfied.  Max value of")
		print(" supersmoother used ")
		ind <- i1[zvar1$y[i1] == max(zvar1$y[i1])]
		option == 1
	}
	if(length(ind) == 0 && (option == 1)) {
		print(" cannot fit variogram")
		error <- 1
		return(error)
	}
	rang1 <- zvar1$x[ind]
	# range
	if(option != 4) sill <- zvar1$y[ind]
	# sill
	xvar2 <- (1.5 * vagram$dist[1:ind])/rang1 - (0.5 * vagram$dist[1:ind]^
		3)/rang1^3
	zvar2 <- vagram$vario[1:ind] - sill * xvar2[1:ind]
	xvar2 <- 1 - xvar2
	nugget <- lsfit(xvar2, zvar2,  , F)$coef
	# 	if the nugget effect is estimated less than 0 it set to 0.05  
	if(nugget < 0) {
		nugget <- 0.05
	}
	error <- 0
	return(list(nugget = nugget, dist = dist, range = rang1, sill = sill,
		error = error))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
