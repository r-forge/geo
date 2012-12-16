\name{rect2deg}

\alias{rect2deg}
\alias{sr2d}
\alias{r2d}

\title{Given statistical rectangle or subrectangle return its center position.}

\description{

  Functions that convert statistical rectangle codes under the
  traditional Icelandic coding system ('tilkynningaskyldurreitakerfid')
  coding systems (results of functions \code{\link{d2r}} and \code{\link{d2sr}}) to
  decimal representation of the rectangles center positions in degreees
  latitude and longitude.

}

\usage{

sr2d(sr)
r2d(r)

}

\arguments{

  \item{sr}{rectangle code \code{sr}, e.g. from \code{\link{deg2rect}}}
  \item{r}{rectangle code \code{r}, e.g from \code{\link{deg2rect}}}      

}

\details{
 \itemize{

  \item \code{sr2d} with a resolution of 15 min latitude by 30 min
  longitude in the Icelandic numbering system for statistical rectangles
  which starts counting at 60 deg N latitude, with sub-rectangles of 30
  min lat by 1 deg lon coded 1, 2, 3 and 4 for the NW, NA, SW and SA
  quadrants respectively,

  \item \code{r2d} with a resolution of 30 min latitue and 1 deg
  longitude (the Icelandic numbering system).

}
}

\value{dataframe of center positions (latitude \code{lat} and longitude
  \code{lon}) of rectangles in one of the coding systems  

}

\note{Mostly used for plotting.}

\author{

  HB

}

\seealso{\code{\link{deg2rect}}}

\examples{
  r2d(d2r(lat = 65 + 1/4, lon = -19 - 1/2))
  d2r(r2d(519))
  
}

\keyword{arith}
\keyword{manip}

