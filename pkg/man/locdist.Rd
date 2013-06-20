\name{locdist}
\alias{locdist}

\title{Distance between two locations}

\description{
Distance between two locations set out by clicking on a geo--plot.
}

\usage{
locdist(scale = "nmi", type = "p")
}

\arguments{
  \item{scale}{Unit of returned distance, default "nmi" for nautical miles, all other values return kilometers}
  \item{type}{Display points "p" or lines "l" between clicks, "n" for nothing}
}

\value{
Returns distance between two point clicks on a geoplot.
}

\note{
Rather limited functionality, could be built further?
}

\seealso{
Calls \code{\link{arcdist}} and \code{\link{geolocator}}.
}

\keyword{iplot}
