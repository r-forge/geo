\name{eyjar}
\alias{eyjar}
\docType{data}
\title{ ~~ data name/kind ... ~~}
\description{
  ~~ A concise (1-5 lines) description of the dataset. ~~
}
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
  country = bisland, grid = F)
geolines(eyjar, col = "magenta")
## maybe str(eyjar) ; plot(eyjar) ...
}
\keyword{datasets}

