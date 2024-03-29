\name{inside}
\alias{inside}

\title{Finds a subset of data inside (or outside) a region}

\description{Finds a subset of data within or without a region, returned as submatrix, boolean vector or indices of the original data.
}

\usage{
inside(lat, lon = 0, reg, option = 1, projection = "Mercator")
}

\arguments{
  \item{lat, lon}{Latitude and longitude of the data, if \code{lat = 0}, \code{lat} must have components \code{lat, lon} or \code{x, y} in case projection is \code{"none"}.}
  \item{reg}{Region we want to check wheter or not includes the data.}
  \item{option}{How should the results be returned:
   \describe{
    \item{1}{Submatrix of data inside \code{reg}}
    \item{2}{Submatrix of data outside \code{reg}}
    \item{3}{Boolean vector, TRUE for data inside \code{reg}}
    \item{4}{Boolean vector, TRUE for data outside \code{reg}}
    \item{5}{Indices of data inside \code{reg}}
    \item{6}{Indices of data outside \code{reg}}}
   }
  \item{projection}{Projection, default \code{"none"}}
}

\value{
Submatrix, boolean vector or index to data as described for argument \code{option}.
}

\note{
Needs elaboration, why is this needed in addition to \code{geoinside}?
}

\seealso{
Called by \code{\link{combine.rt}}, \code{\link{setgrid}}, calls \code{\link{adapt}}.
}

\keyword{manip}
