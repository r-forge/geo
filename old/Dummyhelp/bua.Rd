\name{bua}
\alias{bua}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bua(nm = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nm}{ ~~Describe \code{nm} here~~ }
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
function(nm = 10)
{
	rrt <- c(0, 0, 1, 1)
	crt <- c(0, 1, 1, 0)
	for(i in 2:nm) {
		stdrrt <- c(matrix(0, 8 * i - 4, 1))
		stdcrt <- stdrrt
		n <- i * 8 - 4
		stdrrt[1:(2 * i)] <- 1 - i
		stdrrt[(2 * i + 1):(4 * i - 1)] <- c((2 - i):i)
		stdrrt[(4 * i - 1):(6 * i - 2)] <- i
		stdrrt[(6 * i - 2):(8 * i - 4)] <- c(i:(2 - i))
		stdcrt[1:(2 * i)] <- (c(1 - i):i)
		stdcrt[(2 * i + 1):(4 * i - 1)] <- i
		stdcrt[(4 * i - 1):(6 * i - 2)] <- c(i:(1 - i))
		stdcrt[(6 * i - 2):(8 * i - 4)] <- 1 - i
		crt <- c(crt, stdcrt)
		rrt <- c(rrt, stdrrt)
	}
	i1 <- 4
	for(i in 2:nm) {
		i1[i] <- i1[i - 1] + 8 * i - 4
	}
	i1 <- c(0, i1)
	# 	Part that comes instead of the loop that
	#	is too slow.  
	ind <- c(3, 0, 2, 0, 4, 0, 1)
	r1 <- rrt - 0.1
	c1 <- crt - 0.1
	rr <- sign(r1) + sign(c1) * 2 + 4
	dir <- ind[rr]
	#	dir<- c(1:length(rrt))
	#	for(i in 1:length(rrt)){
	#		if(rrt[i]>0 && crt[i]>0)dir[i]<-1
	#		if(rrt[i]<= 0 && crt[i]>0)dir[i]<-4
	#		if(rrt[i]<= 0 && crt[i]<=0 )dir[i]<-3
	#		if(rrt[i]>0 && crt[i]<=0)dir[i]<-2
	#	}
	return(list(rrt = rrt, crt = crt, dir = dir, i1 = i1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
