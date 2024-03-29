\name{mercator}
\alias{mercator}

\title{Mercator projection}

\description{
Mercator projection.
}

\usage{
mercator(lat, lon, scale = "km", b0 = 65)
}

\arguments{
  \item{lat, lon}{Coordinates as latitude and longitude vectors}
  \item{scale}{Scale of the output, "km" default, all other values imply nautical miles.}
  \item{b0}{Latitude defining the projection.}
}

\value{
List of components:
  \item{lat, lon }{Coordinates in latitude and longitude}
  \item{x, y}{Input coordinates (projected)}
  \item{scale}{Scale}
  \item{projection}{Projection (stated redunantly)}
  \item{b0, L0}{Defining lat and a null value ???}
is returned invisibly.
}

\note{
Needs elaboration, could/should (?) be documented with other projection functions.
}

\seealso{
Called by \code{\link{geoarea}}, \code{\link{gridaxes}} and \code{\link{Proj}}.
}

\keyword{manip}
