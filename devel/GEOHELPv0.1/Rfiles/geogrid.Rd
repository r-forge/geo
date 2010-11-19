\name{geogrid}
\alias{geogrid}
\title{
  Plots a grid.
}
\description{
Plots a grid defined by the vectors lat,lon. The  grid  is
plotted  on a graph initialized by geoplot.  lon gives the
meridians plotted and lat the paralells plotted.
}
\usage{
geogrid(lat, lon=0, col=1, type="l", lwd=0, lty=0, pch="+")
}
\arguments{
\item{lat,lon}{
Latitude  and  longitude  of  data  (  or  x  and  y
       coordinates),  negative for southern latitudes and western
       longitudes.  May be  supplied  as  two  vectors  or  as  a
       dataframe lat (or x) including vectors lat\$lat and lat\$lon
       (x\$x and x\$y if projection = none).
}
\item{col}{
Color number used, default value is 1 (black).
}
\item{type}{
"l" means line and "p" points.  Default is "l".
}
\item{lwd}{
Linewidth.  Default  value  is  the  value  set  when  the
       program was called.
}
\item{lty}{
Linetype.  Default value is the value set when the program
       was called.
}
\item{pch}{
Type of symbol at gridpoints default is "+".
}
}
\value{
No values returned.
}
\seealso{
\code{\link{geoplot}},
\code{\link{geolines}},
\code{\link{geopolygon}},
\code{\link{geotext}},
\code{\link{geosymbols}},
\code{\link{geopar}},
\code{\link{geolocator}},
\code{\link{geocontour}}.
}
\examples{
       geogrid(latgr,longgr)

       codgrd <- list(lat=seq(62,68,by=0.1),lon=seq(-28,-10,0.25))
       geogrid(codgrd)   # a fine grid of Iceland and neighbouring seas
       geoplot(new=T)   
}
% docclass is function
% Converted by Sd2Rd version 43268.
