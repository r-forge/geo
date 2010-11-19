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
geolocator(type=" ", n=0)
}
\arguments{
\item{type}{
Same parameter as in the  locator  function.   Type="l"
       draws line between points.
}
\item{n}{
Number of points. Default value is zero,  then  the  point
       coordinates  are  located till the mouse' middle button is
       clicked.
}
}
\value{
A list with components \$lat  and  \$lon.   or  (\$x,  \$y  if
       geopar\$projection = "none")
}
\seealso{
\code{\link{geoplot}},
\code{\link{locator}},
\code{\link{geodefine}}.
}
% docclass is function
% Converted by Sd2Rd version 43268.
