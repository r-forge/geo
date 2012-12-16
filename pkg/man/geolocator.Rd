\name{geolocator}
\alias{geolocator}
\title{
  Locates points on a plot initialized by geoplot.
}
\description{
The function locates  points  on  a  plot  initialized  by
       geoplot returning their latitude and longitude.
}
\usage{
geolocator(type = "p", n = 0)
}
\arguments{
\item{type}{
Same parameter as in the  locator  function.   Type = "l"
       draws line between points.
}
\item{n}{
Number of points. Default value is zero,  then  the  point
       coordinates  are  located till the mouse' middle button is
       clicked.
}
}
\value{
A list with components \code{$lat}  and  \code{$lon}.   or  (\code{$x,  $y}  if
       \code{geopar$projection = "none"})
}
\seealso{
\code{\link{geoplot}},
\code{\link{locator}},
\code{\link{geodefine}}.
}

