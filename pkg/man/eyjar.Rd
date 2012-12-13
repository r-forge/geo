\name{eyjar}
\alias{eyjar}
\docType{data}
\title{Icelandic islands}
\description{Islands around Iceland with area greater than 0.25 square
  km}

\usage{data(eyjar)}
\format{
  A data frame with 3401 (including NAs) observations on the following 2 variables.

  \describe{
    \item{\code{lat}}{a numeric vector}, \item{\code{lon}}{a numeric
    vector}: latitude and longitude of coastlines of Icelandic islands
    given as decimal degrees.
  }
}

\details{ Islands around Iceland extracted from \code{gshhs_f.b} with
area greater than 1/4 of a square kilometer. Script available as
\url{http://www.hafro.is/~sigurdur/gshhsIslands/newEyjar.R}.  }

\source{ GSHHS - A Global Self-consistent, Hierarchical, High-resolution
Shoreline Database, version 2.2.  }

\examples{
data(eyjar)
# islands in the Breidafjordur region
geoplot(xlim = list(lat = c(64.85, 65.65), lon = c(-24.6, -21.7)),
  country = bisland, grid = FALSE)
geolines(eyjar, col = "magenta")
## maybe str(eyjar) ; plot(eyjar) ...
}
\keyword{datasets}

