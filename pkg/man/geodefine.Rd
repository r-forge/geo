\name{geodefine}
\alias{geodefine}
\title{
  Defines regions.
}
\description{
locates points pointed out by users and defines regions.
}
\usage{
geodefine(nholes=0)
}
\arguments{
\item{nholes}{
The number of holes in data, number of regions - 1.
}
}
\value{
A list of the points pointed out by  the  user  with  NA's between regions.
}
\section{Side Effects}{
Draws the regions on the plot.
}
\details{
Draws the regions on the plot.
}
\seealso{
\code{\link{geolocator}}.
}
\examples{
 ##   Push left mouse button to mark point, push middle button to 
 ##   mark the end of a region.
}
% docclass is function
% Converted by Sd2Rd version 43268.
