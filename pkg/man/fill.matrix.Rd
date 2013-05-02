\name{fill.matrix}
\alias{fill.matrix}

\title{Relace elements of a matrix}

\description{
Replace (fill) elements of a matrix (or data.frame) with a value for given pairs of row and column indices.
}

\usage{
fill.matrix(outcome, x, rownr, dalknr)
}

\arguments{
  \item{outcome}{Input matrix/data.frame}
  \item{x}{Value or values to replace/fill with}
  \item{rownr}{Row index/indices}
  \item{dalknr}{Column index/indices}
}

\value{
Matrix or data.frame with given values replaced.
}

\note{
Probably redundant, not called by any geo-function, the same effect could be achieved with an assignment to a matrix with an index-matrix of the values in rownr and dalknr: \code{mat[matrix(c(rownr, dalknr), ncol = 2)] <- x}
}

\keyword{manip}
