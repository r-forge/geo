\name{arcdist}
\alias{arcdist}
\title{
  Geographical distance computations
}
\description{
Computes distances between lat/lon data points.
}
\usage{
arcdist(lat, lon, lat1=NULL, lon1=NULL)
}
\arguments{
\item{lat}{
Latitude of first coordinate or list with lat, lon of
       first        coordinate.
}
\item{lon}{
  Longitude of first coordinate or list with  lat,  lon
       of second        coordinate.
}
\item{lat1,lon1}{
If lat and lon are vectors of  lat,lon  positions,
       then         lat1 and lon1 must be given as the second set
       of positions.
}
}
\value{
A single vector of distances between pairs of points is returned
}
\seealso{
\code{\link{geoplot}},
\code{\link{geotext}},
\code{\link{selpos}},
}
\examples{
\dontrun{    dists<-arcdist(pos1,pos2)         # pos1 and pos2 are lists of coordinates.
    dists<-arcdist(lat,lon,lat1,lon1) # Input in vector format.
}
}
% docclass is function
% Converted by Sd2Rd version 43268.
