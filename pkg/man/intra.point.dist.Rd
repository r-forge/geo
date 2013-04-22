\name{intra.point.dist}
\alias{intra.point.dist}

\title{
Intra point/position distance
}
\description{
For a data frame of positions return the vector of intra point distances
}
\usage{
intra.point.dist(x)
}

\arguments{
  \item{x}{
List of positions with components \code{lat} and \code{lon}.
}
}
\value{
Vector of distances between the points in \code{x}.
}

\seealso{
\code{\link{arcdist}}
}
\examples{
# distances along the perimeter of a statistical rectangle
pos <- rPeri(323)
intra.point.dist(pos)
sum(intra.point.dist(pos))
}

\keyword{arith}
