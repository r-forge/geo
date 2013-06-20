\name{ps}
\alias{ps}

\title{Periodic spline}

\description{
Fits periodic spline to data.
}

\usage{
ps(x, df = NULL, knots = NULL, period = c(0, 2 * pi))
}

\arguments{
  \item{x}{Data ?}
  \item{df}{Degrees of freedom ?}
  \item{knots}{Knots ?}
  \item{period}{Period, default \code{c(0, 2*pi)} ?}
}

\value{
  \item{basis}{Basis of periodic spline ?}
}

\note{
Needs elaboration. 
}

\seealso{
Called by \code{\link{Closed.curve}}, calls \code{\link[splines]{spline.des}}.
}

\keyword{smooth}
