\name{invlambert}
\alias{invlambert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
invlambert(x, y, lat0, lon0, lat1, scale = "km", old = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{lat0}{ ~~Describe \code{lat0} here~~ }
  \item{lon0}{ ~~Describe \code{lon0} here~~ }
  \item{lat1}{ ~~Describe \code{lat1} here~~ }
  \item{scale}{ ~~Describe \code{scale} here~~ }
  \item{old}{ ~~Describe \code{old} here~~ }
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
function(x, y, lat0, lon0, lat1, scale = "km", old = F)
{
	a <- 6378.388
	# radius at equator
	e <- sqrt(2/297 - (1/297)^2)
	# eccensitret.
	lat11 <- lat1
	# temporary storage
	# 	change to radians  
	lat1 <- (lat1 * pi)/180
	lat0 <- (lat0 * pi)/180
	lon0 <- (lon0 * pi)/180
	#	one or two touching points.  
	if(length(lat1) == 2) {
		lat2 <- lat1[2]
		lat1 <- lat1[1]
		np <- 2
	}
	else np <- 1
	m1 <- cos(lat1)/sqrt(1 - e * e * (sin(lat1))^2)
	if(old) {
		t1 <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat1)))
		t0 <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat0)))
	}
	else {
		t1 <- tan(pi/4 - lat1/2)/((1 - e * sin(lat1))/(1 + e * sin(
			lat1)))^(e/2)
		t0 <- tan(pi/4 - lat0/2)/((1 - e * sin(lat0))/(1 + e * sin(
			lat0)))^(e/2)
	}
	# one tangent.   
	if(np == 1) n <- sin(lat1) else {
		m2 <- cos(lat2)/(1 - e * e * (sin(lat2))^2)
		if(old)
			t2 <- tan(pi/4 - 1/2 * atan((1 - e * e) * tan(lat2)))
		else t2 <- tan(pi/4 - lat2/2)/((1 - e * sin(lat2))/(1 + e *
				sin(lat2)))^(e/2)
		n <- (ln(m1) - ln(m2))/(ln(t1) - ln(t2))
	}
	F1 <- m1/(n * t1^n)
	p0 <- a * F1 * t0^n
	p <- sign(n) * sqrt(x^2 + (p0 - y)^2)
	theta <- atan(x/(p0 - y))
	t <- (p/(a * F1))^(1/n)
	lon <- theta/n + lon0
	lat <- pi/2 - 2 * atan(t)
	for(i in 1:2) {
		# very rapid convergence in all cases.  
		lat <- pi/2 - 2 * atan(t * ((1 - e * sin(lat))/(1 + e * sin(
			lat)))^(e/2))
	}
	return(invisible(list(lat = (lat * 180)/pi, lon = (lon * 180)/pi, x = x,
		y = y, scale = scale, projection = "lambert", lat0 = (lat0 *
		180)/pi, lon0 = (lon0 * 180)/pi, lat1 = lat11)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
