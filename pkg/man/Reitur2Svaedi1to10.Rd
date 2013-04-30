\name{Reitur2Svaedi1to10}
\alias{Reitur2Svaedi1to10}

\title{
Allocate statistical rectangles and subrectangles to Bormicon-areas 1--10.
}
\description{
Routine for allocating rectangles to areas with some in-built decisions for rectangles on the borders of areas.
}
\usage{
Reitur2Svaedi1to10(reitur, smareitur)
}

\arguments{
  \item{reitur}{
Rectangle in the Icelandic statistical rectangle system.
}
  \item{smareitur}{
Sub-rectangle in the Icelandic statistical rectangle system.
}
}
\value{
Returns vector of bormicon areas of the rectangles.
}
\note{
Objects \code{Totalreitir} containing a list of all rectangles competely contained in bc-areas and \code{Dypisreitir} with something are necessary for the function to work.
}

\seealso{
Bormicon-area allocation functions \code{\link{inside.reg.bc}} and \code{\link{inside.reg.bc1}}, \code{\link{r2d}}
}
\keyword{manip}
