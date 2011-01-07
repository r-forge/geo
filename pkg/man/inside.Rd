\name{inside}
\alias{inside}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inside(lat, lon = 0, reg, option = 1, projection = "Mercator")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{ ~~Describe \code{lat} here~~ }
  \item{lon}{ ~~Describe \code{lon} here~~ }
  \item{reg}{ ~~Describe \code{reg} here~~ }
  \item{option}{ ~~Describe \code{option} here~~ }
  \item{projection}{ ~~Describe \code{projection} here~~ }
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
function(lat, lon = 0., reg, option = 1., projection = "Mercator")
{
	# temporary copy
	tmp <- lat
	if(length(lon) < 2. & length(lat) >= 2.) {
		if(projection == "none") {
			lon <- lat$y
			lat <- lat$x
		}
		else {
			lon <- lat$lon
			lat <- lat$lat
		}
	}
	if(length(reg$lat) == 2. && length(reg$lon) == 2.) {
		# rectangular limits 2 pts.
		la <- range(reg$lat)
		lo <- range(reg$lon)
		reg <- list(lat = c(la[1.], la[1.], la[2.], la[2.], la[1.]),
			lon = c(lo[1.], lo[2.], lo[2.], lo[1.], lo[1.]))
	}
	n <- length(reg$lat)
	# has to be closed
	if(n > 0.) if(reg$lat[1.] != reg$lat[n] | reg$lon[1.] != reg$lon[n]) {
			reg$lat <- c(reg$lat, reg$lat[1.])
			reg$lon <- c(reg$lon, reg$lon[1.])
		}
	indx <- c(1.:length(lat))
	indx <- indx[is.na(lat)]
	#outside borders. 
	if(length(indx) > 0.) {
		lat[indx] <- 70.
		lon[indx] <- -60.
	}
	if(projection == "none")
		border <- adapt(reg$x, reg$y)
	else border <- adapt(reg$lat, reg$lon)
	inni <- rep(0., length(lat))
	a <- a1 <- rep(0., length(reg$lat))
	if(!is.loaded(symbol.C("marghc1")))
		dyn.load(paste(DYN.HOME, "margh.new.o", sep = ""))
	inni <- .C("marghc1",
		as.single(lon),
		as.single(lat),
		as.integer(length(lat)),
		as.single(border$lon),
		as.single(border$lat),
		as.integer(length(border$lat)),
		as.integer(border$lxv),
		as.integer(length(border$lxv)),
		as.integer(inni),
		as.single(a),
		as.single(a1))
	inni <- inni[[9.]]
	ind <- c(1.:length(inni))
	ind <- ind[inni > 0.]
	if(option == 1.) {
		if(is.data.frame(tmp)) {
			tmp <- tmp[ind,  ]
			return(tmp)
		}
		else {
			lat <- lat[ind]
			lon <- lon[ind]
			if(projection == "none")
				return(x = lat, y = lon)
			else return(lat, lon)
		}
	}
	else if(option == 2.) {
		if(is.data.frame(tmp)) {
			tmp <- tmp[ - ind,  ]
			return(tmp)
		}
		else {
			lat <- lat[ - ind]
			lon <- lon[ - ind]
			if(projection == "none")
				return(x = lat, y = lon)
			else return(lat, lon)
		}
	}
	else if(option == 3.)
		return(inni)
	else if(option == 5.) {
		ind <- c(1.:length(inni))
		ind <- ind[inni == 0.]
		return(ind)
	}
	else return(ind)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
