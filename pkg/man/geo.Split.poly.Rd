\name{geo.Split.poly}
\alias{geo.Split.poly}

\title{Split a polygon into a list}

\description{
Split a 'polygon with NAs' into a list of true polygons before calculating its area.
}

\usage{
geo.Split.poly(data)
}

\arguments{
  \item{data}{A dataframe of coordinates in latitude and longitude}
}

\value{
Returns a list of true polygons as dataframes, each with coordinates 
  \item{lat}{Latitude}
  \item{lon}{Longitude}
}

\note{
Needs further elaboration.
}

\seealso{ 
Called by \code{\link{geoarea}}.
}

\keyword{manip}
