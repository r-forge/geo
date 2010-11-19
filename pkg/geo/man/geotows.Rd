\name{geotows}
\alias{geotows}
Plot tows
\title{
Plot line segments from lat,lon to lat1,lon1
}
\description{
the function gets 4 arguments i.e position of begininning and end of
segments.  If the first argument is given col.names gives the column
names in the data frame describing the position.   
}
\usage{
geotows(lat, lon, lat1, lon1, col=1, col.names = c("kastad.n.breidd", "kastad.v.lengd", "hift.n.breidd", "hift.v.lengd"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{lon}{
%%     ~~Describe \code{lon} here~~
}
  \item{lat1}{
%%     ~~Describe \code{lat1} here~~
}
  \item{lon1}{
%%     ~~Describe \code{lon1} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{col.names}{
%%     ~~Describe \code{col.names} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(lat, lon, lat1, lon1, col=1,col.names = c("kastad.n.breidd", 
	"kastad.v.lengd", "hift.n.breidd", "hift.v.lengd"), ...)
{
	if(is.data.frame(lat) && missing(lat1)) {
		lat1 <- lat[, col.names[3]]
		lon1 <- lat[, col.names[4]]
		lon <- lat[, col.names[2]]
		lat <- lat[, col.names[1]]
	}
	if(is.data.frame(lat) && !missing(lat1)) {
		lon <- lat$lon
		lat <- lat$lat
		lon1 <- lat1$lon
		lat1 <- lat1$lat
	}
	lat <- matrix(lat, length(lat), 3)
	lat[, 2] <- lat1
	lat[, 3] <- NA
	lat <- c(t(lat))
	lon <- matrix(lon, length(lon), 3)
	lon[, 2] <- lon1
	lon[, 3] <- NA
	lon <- c(t(lon))
	geolines(lat, lon,col=col, ...)
	return(invisible())
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
