\name{giveborder}
\alias{giveborder}

\title{Define regions intteractively}

\description{
Define regions interactively.
}

\usage{
giveborder(nholes = 0)
}

\arguments{
  \item{nholes}{The number of holes in the data, number of regions - 1}
}

\value{
Returns a composite list of the following compents:
  \item{reg}{List with components number of components (?)}
  \item{x, y}{Coordinates when \code{geopar$projection == "none"}}
  \item{lat, lon}{Geographical coordinates for other projections (Mercator and Lambert (?)}
  \item{lxv}{ID of region (?)}
}

\note{
Needs further elaboration.
}

\seealso{
Called by \code{\link{geodefine}}, calls \code{\link{invProj}}.
}

\keyword{iplot}
