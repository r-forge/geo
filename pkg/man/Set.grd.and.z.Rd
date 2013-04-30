\name{Set.grd.and.z}
\alias{Set.grd.and.z}

\title{Manipulate grid and z-values for contouring.}

\description{
Manipulate grid and z-values for contouring.
}

\usage{
Set.grd.and.z(grd, z, mask, set = NA, col.names = c("lon", "lat"))
}

\arguments{
  \item{grd}{grid}
  \item{z}{z-value}
  \item{mask}{mask}
  \item{set}{set}
  \item{col.names}{column names, defaults to \code{lat} and \code{lon}}
}

\value{
Returns a list of:
  \item{grd}{grid}
  \item{z}{value over grid}
}

\note{
Used in \code{geocontour}-functions.
}

\keyword{manip}
