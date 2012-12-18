\name{rectArea}
\alias{rectArea}
\alias{rA}
\alias{srA}
\alias{mrA}
\alias{drA}

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
mrA(mr, dlat = 5, dlon = 10, scale = "nmi")
drA(dr, dlat = 1, dlon = 2, scale = "km")
}

\arguments{

\item{r, sr, mr, dr}{
  rectangle code, as in \code{\link{rect2deg}} and
  \code{\link{deg2rect}}.
}
\item{scale}{
  \code{nmi, km}, default \code{nmi} returns values 
  in square nautical miles for all except \code{drA} returns 
  area in square kilometers.
}
\item{dlat, dlon}{
  Dimensions of latitude and longitude given in minutes
  and degrees for \code{mrPeri} and \code{drPeri}, respectively.
}

}

\value{Rectangle area in square nautical miles or kilometers.}

\note{Unit \code{nmi} is used for historical/acoustical (sA) reasons.}

\seealso{\code{\link{rectPeri}}, \code{\link{deg2rect}}, \code{\link{geoarea}}.}

\examples{
  srA(7121)
  srA(7121, "km")
  srA(7121, "km")/1.852^2
  srA(7121, "km")
  rA(712)
  srA(7121) + srA(7122) + srA(7123) + srA(7124)
}

\keyword{arith}
\keyword{manip}
