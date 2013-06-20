\name{pltgrid}
\alias{pltgrid}

\title{Plots grid lines}

\description{
Plots grid lines.
}

\usage{
pltgrid(xgrid = NULL, ygrid = NULL, xpos, ypos, ...)
}

\arguments{
  \item{xgrid}{Should x-grid be drawn, default NULL?}
  \item{ygrid}{Should y-grid be drawn, default NULL?}
  \item{xpos}{xpos ?, if missing taken from \code{par("xaxp")}}
  \item{ypos}{ypos ?, if missing taken from \code{par("yaxp")}}
  \item{\dots}{optional parameters to be sent to \code{lines}}
}

\value{
No value, draws grid lines on current plot.
}

\note{
Needs elaboration. Prints ypos ??
}

\seealso{
Neither called by nor calls any function in package geo.
}

\keyword{aplot}
