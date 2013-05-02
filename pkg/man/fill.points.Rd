\name{fill.points}
\alias{fill.points}

\title{Fill points (thicken)}

\description{
Fill points (thicken) for drawing continous lines in Lambert projection.
}

\usage{
fill.points(x, y, nx, option = 1)
}

\arguments{
  \item{x, y}{Coordinates}
  \item{nx}{Thickening factor}
  \item{option}{Deals with NAs in the coordinates when not 1, the default}
}

\value{
List of thickened values with components:
  \item{x, y}{of coordinates}
}

\note{
Internal, needs elaboration.
}

\seealso{
The function is called by \code{\link{geopolygon}}, \code{\link{geolines}}, \code{\link{reitaplott}} and \code{\link{gridaxes.Lambert}}
}

\keyword{manip}
