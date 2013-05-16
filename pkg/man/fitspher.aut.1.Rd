\name{fitspher.aut.1}
\alias{fitspher.aut.1}

\title{Fits a super-smoother to a variogram (?)}

\description{
Fits a super-smoother to a variogram (?).
}

\usage{
fitspher.aut.1(vagram, option, sill)
}

\arguments{
  \item{vagram}{Variogram}
  \item{option}{What to do ?}
  \item{sill}{Sill for the variogram (?)}
}

\value{
Returns a list with components of the returned variogram:
  \item{nugget}{Nugget}
  \item{dist}{Distance}
  \item{range}{Range}
  \item{sill}{Sill}
  \item{error}{Does the function call return an error? Used in \code{variofit}}
}

\note{
Needs further elaboration. 
}

\seealso{
Called by \code{\link{variofit}}.
}

\keyword{smooth}
