\name{Open.curve}
\alias{Open.curve}

\title{Open smooth curve through positions}

\description{
A smooth curve is fitted to the positions with natural spline as implemented in function \code{ns}.
}

\usage{
Open.curve(gogn, df = nrow(gogn)/2, n = 10)
}

\arguments{
  \item{gogn}{Data frame of positions to be smoothed}
  \item{df}{Degrees of freedom}
  \item{n}{Number of points returned per data point}
}

\value{
Data frame of positions with components
  \item{lat, lon}{in decimal degrees}
}

\note{
Function \code{ns} is missing, assume it is some sort of natural spline.
}

\keyword{manip}
