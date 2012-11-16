\encoding{latin1}

\name{degrees2rectangles}
\alias{degrees2rectangles}
\alias{d2r}
\alias{d2sr}

\title{Given position return statistical rectangle or subrectangle
  (quadrants of statistical rectangles)}

\description{

  Functions that convert positions in decimal degrees latitude and
  longitude to rectangle codes for statistical rectangles or
  subrectangles as described below and relating to
  \link{\code{rectangles2degrees}}.

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

\note{\code{d2dr} these functions could be made hemisphere-aware, and no
  attention has been given to making the functions work in the southern
  hemisphere, possibly with the option of specifying starting
  latitudes.}

\author{

  HB

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  \code{rectangles2degrees}
%% \link{\code{rectangles2degrees}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{

## tally positions in rectangles in object \code{island} giving Iceland
   coastline

data(island)
rects <- d2r(island)
table(rects)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{arith}

