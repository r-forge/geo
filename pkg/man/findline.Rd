\name{findline}
\alias{findline}

\title{Finds a line (?)}

\description{
Finds a line (?).
}

\usage{
findline(x, xb, plot = T)
}

\arguments{
  \item{x}{Coordinates 1 (?)}
  \item{xb}{Coordinates 2 (?)}
  \item{plot}{Plot or not, default TRUE}
}

\value{
If plot is TRUE, returns coordinates for plotting, if FALSE returns
data.frame of with latitude and longitude.
}

\note{
Needs further elaboration.
}

\seealso{
Called by \code{\link{adjust.grd}}, \code{\link{geolines}},
\code{\link{geolines.with.arrows}}, \code{\link{init}} and
\code{link{reitaplott}}, calls \code{\link{invProj}},
\code{\link{prepare.line}} and \code{\link{Proj}}.
}

\keyword{manip}
