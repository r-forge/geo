\name{rectArea}
\alias{rectArea}
\alias{srA}
\alias{rA}

\title{
Given rectangle code return area in square kilometers or nautical miles
}

\description{
Rectangle area is returned through a call to
\code{\link{rectPeri}}-functions and \code{\link{geoarea}}.
}

\usage{
rA(r, scale = "nmi")
srA(sr, scale = "nmi")
}

\arguments{
  \item{r, sr}{rectangle code}, as in \code{\link{rect2deg}} and
  \code{\link{deg2rect}}.
  \item{scale}{default \code{nmi}, returns values in square nautical miles, 
  any other value returns area in square kilometers}
}

\value{rectangle area in square nautical miles or kilometers}

\seealso{\code{\link{rectPeri}}, \code{\link{deg2rect}}, \code{\link{geoarea}}.}

\examples{
  srA(7121)
  srA(7121, "km")
  srA(7121, "km")/1.852^2
  srA(7121, "km2")
  rA(712)
  srA(7121) + srA(7122) + srA(7123) + srA(7124)
}

\keyword{arith}
\keyword{manip}


