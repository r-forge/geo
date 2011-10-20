\name{geoexpand}
\alias{geoexpand}
\title{
  expands a grid to a dataframe.
}
\description{
Expands a grid to a dataframe simular to expand.grid.
}
\usage{
geoexpand(grid)
}
\arguments{
\item{grid}{
The grid to be expanded, a list containing vectors \$lat
       and \$lon.
}
}
\value{
A dataframe of the grid.
}
\section{Side Effects}{
None.
}
\seealso{
\code{\link{expand.grid}}.
}
\examples{
       grd <- list(lat=seq(63,67,length=30),lon=seq(-28,-10,length=50))
       # a list with length(lat) = 30 and length(lon) = 50

       grd1<- geoexpand(grd)
       # a 30 by 50 dataframe made.

       # See also examples in geocontour.
}
% docclass is function
% Converted by Sd2Rd version 43268.
