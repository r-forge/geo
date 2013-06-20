\name{invmerc}
\alias{invmerc}

\title{Inverse Mercator Projection}

\description{
Inverse Mercator Projection.
}

\usage{
invmerc(x, y, scale = "km", b0 = 65)
}

\arguments{
  \item{x, y}{The input data to be inversely projected as two vectors}
  \item{scale}{Scale of the input data, "km" default, all other values imply nautical miles.}
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
Needs elaboration and perhaps documenting with mercator in the same doc-file.
}

\seealso{
Called by \code{\link{invProj}}.
}

\keyword{manip}
