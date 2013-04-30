\name{Closed.curve}
\alias{Closed.curve}

\title{Closed smooth curve through positions}
\description{
A smooth curve is fitted to the positions with periodic spline as implemented in function \code{ps}.
}

\usage{
Closed.curve(gogn, df = round(nrow(gogn)/2), n = 10)
}

\arguments{
  \item{gogn}{Data frame of positions to be smoothed}
  \item{df}{Degrees of freedom}
  \item{n}{Number of points returned per data point}
}

\value{
Data frame of positions with components
  \item{lat, lon }{in decimal degrees}
}
\seealso{
\code{\link{ps}}
}

\note{
Missing function \code{spline.des} is required for \code{\link{ps}} to work.
}

\keyword{manip}
