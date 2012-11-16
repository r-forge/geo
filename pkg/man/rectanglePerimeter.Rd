\name{rectanglePerimeter}
\alias{rectanglePerimeter}
\alias{rPeri}
\alias{srPeri}

\title{
Given rectangle code return perimeter as a polygon in lat lon
}

\description{

  The outline/boundary of a statistical rectangle is returned as 5
  positions, the first and last of which are the same.

}

\usage{
rPeri(r)
srPeri(sr)
}

\arguments{
  \item{r, sr}{rectangle codes}
}

\value{rectangle outline as 5 positions}

\note{should perhaps be extended to give a list or dataframe of polygons
  for more than on \code{r} or \code{sr}.}

\seealso{\code{\link{degrees2rectangles}}}

\examples{

  geoplot(island, type = "n", grid = FALSE)
  geolines(rPeri(468))
  geolines(srPeri(4681))
  
}


\keyword{manip}
\keyword{arith}
