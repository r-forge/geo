\name{plot_reitnr}
\alias{plot_reitnr}

\title{Plots rectangle codes}

\description{
Adds codes for statistical rectangles to the current plot
}

\usage{
plot_reitnr(cexrt, lwd = 0)
}

\arguments{
  \item{cexrt}{Character expansion of the codes on the plot.}
  \item{lwd}{Grid line width}
}

\value{
No value, adds the codes for statistical rectangles to the current geoplot. The codes are added for all rectangles visible on the plot.
}

\note{
Might be extended to plotting 'smareitur' codes? Grid line setting affects bounding box of the plot could be fixed/extended as well(?).
}

\seealso{
Called by \code{\link{geoplot}}, calls \code{\link{d2r}}, \code{\link{geotext}} and \code{\link{invProj}}.
}

\keyword{aplot}
