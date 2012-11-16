\name{rectangles2degrees}

\alias{rectangles2degrees}
\alias{sr2d}
\alias{r2d}

\title{Given statistical rectangle or subrectangle return its center position.}

\description{

  Functions that convert statistical rectangle codes under the
  traditional Icelandic coding system ('tilkynningaskyldurreitakerfið')
  coding systems (results of functions \code{d2r} and \code{d2sr}) to
  decimal representation of the rectangles center positions in degreees
  latitude and longitude.

}

\usage{
dr2d(dr, dlat = 1, dlon = 2)
mr2d(mr, dlat = 5, dlon = 10)
hr2d(hr)
sr2d(sr)
r2d(r)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{sr}{rectangle code \code{sr}, e.g. from \link{\code{d2sr}}}
  \item{r}{rectangle code \code{r}, e.g from \link{\code{d2r}}}      

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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\note{Mostly used for plotting.}

\author{

  HB

}

\seealso{\code{\link{degrees2rectangles}}}

\examples{
  r2d(d2r(lat = 65 + 1/4, lon = -19 - 1/2))
  d2r(r2d(519))
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

