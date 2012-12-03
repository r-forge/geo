\name{rectGrid}
\alias{rgrid}
\alias{srgrid}

\title{

  Plots grid of statistical rectangles

}

\description{

  Plots the outline of or fills statistical rectangles or their
  sub-rectangles with call to \code{\link{geolines}} or
  \code{\link{geopolygon}}.   

}

\usage{

rgrid(r, fill = F, ...)
srgrid(r, fill = F, ...)

}

\arguments{

  \item{r, sr}{
  Rectangle codes to be outlined or filled.
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
  whereas \code{\link{reitaplott}} and \code{\link[geoextras]{geoSR}}
  assume levels.

}

\value{

  No values returned, used for side-effects.

}

\author{

  STJ
}

\note{

  Functions \code{\link{reitaplott}} and \code{\link[geoextras]{geoSR}}
  have more in-built functionality to deal with level-plots of
  recatangles.

}

\seealso{

  \code{\link{deg2rect}}, \code{\link{geolines}},
  \code{\link{geopolygon}}, \code{\link{reitaplott}}.
}

\examples{

## some rectangles on Iceland's coastline

geoplot(grid = FALSE)
tmp <- island
tmp$r <- d2r(island)
tmp$sr <- d2sr(island) 
rects <- aggregate(lat ~ r, tmp, length)
srects <- aggregate(lon ~ sr, tmp, length)
names(rects)[2] <- names(srects)[2] <- "count"
rects$lev <- cut(rects$count, c(0, 1, 10, 20, 50, 100, 1000))
srects$lev <- cut(srects$count, c(0, 1, 5, 10, 20, 50, 100))
mycol <- heat.colors(length(unique(srects$lev)))
srgrid(srects$sr, fill = T, col = mycol[srects$lev])
geolines(island)

}

\keyword{hplot}
\keyword{spatial}
