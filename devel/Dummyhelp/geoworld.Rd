\name{geoworld}
\alias{geoworld}
\title{
  Plots rough outline of the world.
}
\description{
  Plots roughly the outlines of those countries  which  have
       borders  intersecting  the  current  plot  initialized  by
       geoplot.  The countries can also be filled.  
}
\usage{
geoworld(regions=".", exact=F, boundary=T, fill=F, color=1, lwd=1,
         lty=1, plot=T, type="l", pch=".", database="world.thin",
         return.data=F)
}
\arguments{
\item{regions}{
 Allows plotting only a certain part of the  world  to
       speed up the function, such as regions = "iceland". Default is
       plotting what is insidie the map.  
}
\item{exact}{
Draws a more exact plot.
}
\item{boundary}{
 If true country boundaries  are  drawn.   Default  is
       false.
}
\item{fill}{
If true countries are filled with a color.  Default is false.
}
\item{color}{
Default is 1 (usually black).
}
\item{lwd}{
Linewidth, default is 1.
}
\item{lty}{
Linetype, default is 1.
}
\item{plot}{
 If false a plot is not drawn.  Default is true.
}
\item{type}{
 If lines or points  should  be  potted,  see  type  for
       geoplot.
}
\item{pch}{
The character to be used for plotting, see par.
}
\item{database}{
The database the world plot  is  to  be  taken  from.
       Currently  there  are  two  possible  databases  world and
       worldHires.  world is in the map library and worlHires in the
       mapdata library and is much more precies.  
}
\item{return.data}{
 If true those points used to make the in the  plot
       are return, default is F. Can be used to save the coastlines of
       specified countries.  
}
\item{outside=F}{
If T data are plotted outside the borders.  Default is F. 
}
\item{allowed.size}{
Maximum size of polygon that can be filled.  Default is 80 000 which
is rather large polygon but this value is rapidly changing by more
powerful hardware.  
}
}
\value{
default none, see option return data.
}
\section{Side Effects}{
the outlines of all countries who intersect  current  plot
       are drawn.
}
\seealso{
\code{\link{geoplot}},
\code{\link{fill.outside.border}},
\code{\link{geopolygon}},
\code{\link{par}}.
}
\examples{
     geoplot(xlim=c(0,-53),ylim=c(53,75))
     geoworld()

     # Should plot in all countries who intersect the plot draw
     # with geoplot.

# The packages maps and mapdata need to be installed
# worldHires is a very detailed database of coastlines from the 
# package mapdata.  Could be problematic if used with fill=T)
# Allowed.size is the maximum allowed size of polygons.  
library(map) # world coastlines and programs 
library(mapdata) # more detailed coastlines
geoplot(xlim=c(20,70),ylim=c(15,34))
geoworld(database="worldHires",fill=T,col=30,allowed.size=100000) #

geoplot(xlim=c(20,70),ylim=c(15,34),dlat=10,dlon=10)
geoworld(database="world",fill=T,col=30) #

geoplot(xlim=c(-10,70),ylim=c(71,81),b0=80,dlat=2,dlon=10) # 0 must be high here else
geoworld(database="world",fill=T,col=30) #the plot fails.  

# Lambert projection,
geoplot(xlim=c(-10,70),ylim=c(71,81),dlat=2,dlon=10,projection="Lambert")
geoworld(database="world",fill=T,col=30) 
}
% docclass is function
% Converted by Sd2Rd version 43268.
