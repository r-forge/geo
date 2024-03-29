\name{geolines.with.arrows}
\alias{geolines.with.arrows}

\title{Add arrowhead to plot}

\description{
Adds arrowhead at the start or the end of a list of coordinates on a geoplot.
}

\usage{
geolines.with.arrows(data, start = T, size = 0.2, ...)
}

\arguments{
  \item{data}{Dataframe with coordinates in columns \code{lat, lon}.}
  \item{start}{Start from beginning (i.e. arrow points at first coordinate), default TRUE.}
  \item{size}{Size of arrow}
  \item{\dots}{Additional arguments to \code{Arrow}}
}

\value{
Draws an Arrowhead at the start or the end of the coordinates given in \code{data}, returns invisibly the outline as four coordinate pairs in a dataframe.
}

\note{
May need further elaboration and/or detail.
}

\seealso{
Called by \code{\link{geocurve}}, calls \code{\link{findline}}, \code{\link{invProj}} and \code{\link{Arrow}}.
}

\keyword{aplot}
