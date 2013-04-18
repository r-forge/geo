\name{rectGrid}
\alias{rgrid}
\alias{srgrid}
\alias{mrgrid}
\alias{drgrid}

\title{
  Produce a grid of rectangles on a plot, filled with colors if desired.
}

\description{

  Functions that plot a line grid or one filled with colors for 
  rectangle codes in systems of various resolutions with call 
  to \code{\link{geolines}} or \code{\link{geopolygon}} 
  and perimeter utility functions (see \code{\link{rectPeri}}).   

}

\usage{
  rgrid(r, fill = FALSE, ...)
  srgrid(sr, fill = FALSE, ...)
  mrgrid(mr, dlat = 5, dlon = 10, fill = FALSE, ...)
  drgrid(dr, dlat = 1, dlon = 2, fill = FALSE, ...)
}

\arguments{

\item{r, sr, dr, mr}{
  Codes of rectangle to be outlined or filled. In different
  resolutions, \code{r, sr} with their own system 
  (see \code{\link{deg2rect}}), \code{mr} dimensions based
   on minutes, \code{dr} on degrees.
}

\item{dlat, dlon}{
  Dimensions of latitude and longitude given in minutes
  and degrees for \code{mrgrid} and \code{drgrid}, respectively
}

\item{fill}{
  Logical, whether or not to fill the plotted rectangles.
}

\item{\dots}{
  other arguments to \code{\link{geopolygon}} or \code{\link{geolines}}
  as appropriate, notably \code{col}. 
}
}

\details{
  The functions simply outline or fill the rectangles they are given.
  whereas \code{\link{reitaplott}} and \code{\link{geoSR}}
  assume levels and are more hi-level.
}

\value{
  No values returned, used for side-effects.
}

\author{
  STJ
}

\note{
  Functions \code{\link{reitaplott}} and \code{\link{geoSR}}
  have more in-built functionality to deal with level-plots of
  rectangles.
}

\seealso{
  \code{\link{deg2rect}}, \code{\link{geolines}},
  \code{\link{geopolygon}}, \code{\link{rectPeri}},
  \code{\link{reitaplott}}, \code{\link{geoSR}}.
}

\examples{

geoplot(grid = FALSE)
tmp <- island
tmp$sr <- d2sr(island) 
srects <- aggregate(. ~ sr, tmp, length)
names(srects)[2] <- "count"
srects$lev <- cut(srects$count, c(0, 1, 5, 10, 20, 50, 100))
mycol <- heat.colors(length(unique(srects$lev)))
srgrid(srects$sr, fill = TRUE, col = mycol[srects$lev])
geolines(island)

}

\keyword{hplot}
\keyword{spatial}
