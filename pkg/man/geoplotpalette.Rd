\name{geoplotpalette}
\alias{geoplotpalette}

\title{
Set define a traditional geoplot-palette
}
\description{
Set define a traditional geoplot-palette with colors from the matix 'postcol'.
}
\usage{
geoplotpalette()
}

\details{

}
\value{
Sets the palette
}
\references{

}

\note{

}

\seealso{
 \code{\link{colps}}, \code{\link{hsv}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    x <- hsv(postcol[, 1], postcol[, 2], postcol[, 3])
    palette(x)
  }
}

\keyword{ ~kwd1 }

