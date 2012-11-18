\name{rectArea}
\alias{rectArea}
\alias{srA}
\alias{rA}

\title{
Given rectangle code return area in square kilometers or nautical miles
}

\description{
Rectangle area is returned through a call to
\link{\code{rectPeri}}-functions and \link{\code{geoarea}}.
}

\usage{
rA(r)
srA(sr)
}

\arguments{
  \item{r, sr}{rectangle code}, as in \link{\code{rect2deg}} and
  \link{\code{deg2rect}}.
  \item{scale}{default \code{nmi}, returns values in square nautical miles, 
  any other value returns area in square kilometers
}

\value{rectangle area in square nautical miles or kilometers}

\seealso{\link{\code{rectPeri}}, \link{\code{deg2rect}}, \link{\code{geoarea}}.}

\examples{

  srA(7121)
  srA(7121, "km")
  srA(7121, "km")/1.852^2
  srA(7121, "km2")
  rA(712)
  srA(7121) + srA(7122) + srA(7123) + srA(7124)

# pretty close
}

\keyword{arith}
\keyword{manip}


