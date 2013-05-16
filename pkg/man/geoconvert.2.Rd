\name{geoconvert.2}
\alias{geoconvert.2}

\title{Convert from decimal degrees}

\description{
Convert from decimal degrees to degrees, minutes and fractional minutes representation (DDMMmm) of lat or lon.
}

\usage{
geoconvert.2(lat)
}

\arguments{
  \item{lat}{Vector of latitude or longitudes}
}

\value{
Returns a vector of six digit values with degrees, minutes and fractions of minutes, with two decimal values, concatenated.
}

\seealso{
Called by \code{\link{geoconvert}}, when \code{inverse = TRUE}.
}

\keyword{manip}
