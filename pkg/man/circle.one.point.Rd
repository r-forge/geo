\name{circle.one.point}
\alias{circle.one.point}

\title{
Find a circle around a point
}
\description{
Through simple approximate geometry the perimeter of a circle around a point given in lat and lon is found
}
\usage{
circle.one.point(lat, lon = NULL, rad, n = 10)
}

\arguments{
  \item{lat, lon}{Latitude and longitude in decimal degrees.  }

  \item{rad}{ radius of the circle in \code{nmi}}

  \item{n}{ number of points on the circle perimeter }
}

\examples{
## draws a circle around Iceland
geoplot()
geolines(circle.one.point(65,-19,150,n=50),lwd=3,col="red")
}

\keyword{hplot}
