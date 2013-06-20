\name{pdist}
\alias{pdist}

\title{Spherical (?) distance}

\description{
Spherical (?) distance.
}

\usage{
pdist(lat, lon, lat1, lon1)
}

\arguments{
  \item{lat, lon}{Coordinate vectors of starting positions in lat/lon}
  \item{lat1, lon1}{Coordinate vectors of end positions in lat/lon}
}

\value{
Distance in kilometers.
}

\note{
Why not use \code{arcdist}? Distance functins could be documented in one file.
}

\seealso{
Called by \code{\link{pointkriging}} and \code{\link{variogram}}.
}

\keyword{manip}
