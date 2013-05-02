\name{apply.shrink}
\alias{apply.shrink}

\title{Apply a function to a vector for a combination of categories.}

\description{
Apply a function to a vector for a combination of categories.
}

\usage{
apply.shrink(X, INDICES, FUN = NULL, names, ...)
}

\arguments{
  \item{X}{Input data to \code{FUN}}
  \item{INDICES}{list of categories to be combined}
  \item{FUN}{Function to be applied}
  \item{names}{Column names for the resulting dataframe}
  \item{\dots}{Additional arguments to \code{FUN}}
}

\value{
Dataframe of outcomes applying \code{FUN} to \code{X} for the combination of categories in \code{INDICES}
}

\note{
Needs elaboration, or could be dropped/hidden, use merge instead.
}

\seealso{
\code{\link{apply.shrink.dataframe}}, \code{\link{merge}}
}

\examples{
## stupid example, showing naming of results
names(apply.shrink(depthloc$z, 
  list(a=round(depthloc$lat), b=round(depthloc$lon)), mean))
names(apply.shrink(depthloc$z, list(round(depthloc$lat), 
  round(depthloc$lon)), mean, names = c("a", "b", "z")))
}

\keyword{manip}
