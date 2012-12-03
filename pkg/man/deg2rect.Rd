\encoding{latin1}

\name{deg2rect}
\alias{deg2rect}
\alias{d2r}
\alias{d2sr}

\title{Given position return statistical rectangle or subrectangle
  (quadrants of statistical rectangles)}

\description{

  Functions that convert positions in decimal degrees latitude and
  longitude to rectangle codes for statistical rectangles or
  subrectangles as described below and relating to
  \code{\link{rect2deg}}.

}
\usage{

d2r(lat, lon = NULL)
d2sr(lat, lon = NULL)

}

\arguments{

  \item{lat, lon}{position(s) as decimal degrees latitude and
  longitude. If \code{lat} is \code{list} its components \code{lat$lat}
  and \code{lat$lon} are used for \code{lat} and \code{lon}.}

}

\details{

 \itemize{

  \item \code{r2d} with a resolution of 30 min latitue and 1 deg
  longitude (the Icelandic numbering system, 'tilkynningaskyldureitir').

  \item \code{sr2d} with a resolution of 15 min latitude by 30 min
  longitude in the Icelandic numbering system for statistical rectangles
  which starts counting at 60 deg N latitude, with sub-rectangles of 30
  min lat by 1 deg lon coded 1, 2, 3 and 4 for the NW, NA, SW and SA
  quadrants respectively

    }

  A small number (1e-06) is added to latitude and subtracted from
  longitude to ensure rectangle membership of positions on border are
  \dQuote{logical} on the nw-hempisphere.

}
\value{vector of rectangle codes in the chosen coding system.}

\note{These functions could be made hemisphere-aware, and no
  attention has been given to making the functions work in the southern
  hemisphere, possibly with the option of specifying starting
  latitudes.}

\author{

  HB

}


\seealso{

  \code{\link{rect2deg}}

}

\examples{

## tally positions in rectangles in object \code{\link{island}} giving
## Iceland's coastline

data(island)
rects <- d2r(island)
table(rects)

}

\keyword{manip}
\keyword{arith}

