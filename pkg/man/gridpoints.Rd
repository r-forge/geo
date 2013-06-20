\name{gridpoints}
\alias{gridpoints}

\title{Produce gridpoints over an area}

\description{
Produce gridpoints over an area.
}

\usage{
gridpoints(border, dx, grpkt, nx, n)
}

\arguments{
  \item{border}{Border of the area}
  \item{dx}{Resolution in each direction (?)}
  \item{grpkt}{???}
  \item{nx}{Number of gridpoints in each direction (?)}
  \item{n}{Total number of gridpoints (?)}
}

\value{
List with components:
  \item{xgr}{List of gridpoints in components \code{lat, lon} or \code{x,y} depending on the projection.}
  \item{xgra}{List of those gridpoints within the area given in \code{border}}
}

\note{
Needs further elaboration, check use with \code{find = TRUE} in \code{setgrid}.
}

\seealso{
Called by \code{\link{setgrid}}.
}

\keyword{manip}
