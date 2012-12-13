\name{geopolygon}
\alias{geopolygon}
\title{
  Fill an area.
}
\description{
The program fills an area. The program is similar to the polygon function except the data is in lat,lon and the transform of the data specified in geoplot is used. Also there are some additional parameters. The graph has to be initialized by geoplot.
}
\usage{
geopolygon(lat, lon=NULL, col=0, border=F, exterior=F, nx=1,
           outside=F, plot=T, save=F, rat=0.005, density=-1,
           Projection=NULL, angle=45, allowed.size=4000, option=1)
}
\arguments{
\item{lat,lon}{
Latitude and longitude of data ( or x and y coordinates),  negative for southern latitudes and western longitudes. May be supplied as two vectors or as a list lat (or x) including vectors lat\$lat and lat\$lon (x\$x and x\$y if projection = none).
}
\item{col}{
Color number used.  Default value is 0 (often white).
}
\item{border}{
If T borders around the polygon are drawn. Default value is F.
}
\item{exterior}{
If T everything that is outside the polygon is painted , else everything inside. Default value is F. If exterior = T axes and grid often need refreshing by calling geoplot again with new=T.
}
\item{nx}{
See geolines for further details.
}
\item{outside}{
if T what is outside of the polygon is colored else what is inside. Default value is T.
}
\item{plot}{
if T the polygon is plotted. Default is T.
}
\item{save}{
if T the points plotted are returned. Default is F.
}
\item{rat}{
the ratio of the plot to what is plotted. Default is 0.005 meaning that the plot is 0.5\% bigger than what is plotted.
}
\item{density}{
see polygon.
}
\item{Projection}{
the projection to be used. Default is the one defined by current plot.
}
\item{angle}{
see polygon.
}
\item{allowed.size}{
printers are limited to printing polygons of certain size, when the polygons size is actually too big for your printer you can enter the tedious task of splitting up the polygon. Default is 4000.
}
}
\value{
The points plotted are returned if save=T.
}
\seealso{
\code{\link{polygon}},
\code{\link{geoplot}},
\code{\link{geolines}},
\code{\link{geopoints}},
\code{\link{geotext}},
\code{\link{geosymbols}},
\code{\link{geocontour.fill}},
\code{\link{geogrid}},
\code{\link{geocontour}}.
}
\examples{
\dontrun{     geopolygon(island)              # Paint iceland with 
                                     # color #0 (often white).

     geopolygon(island,col=0,exterior=T)

     geopolygon(geolocator(),col=1)  # Paints a region defined 
                                     # by pointing on map black.

     # Of the maps available island (iceland) is about the only that
     # is correctly defined as closed polygon so it is the only one that 
     # can be painted by geopolygon.

     geoplot(grid=F,type="n")
     # Star by setting up the plot.
     geopolygon(gbdypif.500,col=4,exterior=F,r=0)
     # Use geopolygon to draw the 500 m area. 
     geopolygon(gbdypif.100,col=155,exterior=F,r=0)
     # Draw 100 m are over the 500 m. 
     geolines(eyjar,col=115)
     # Add islands around Iceland.
     gbplot(c(100,500),depthlab=T)
     # Draw the depth lines, labels on lines.
     geopolygon(island,col=115,outside=T,r=0)
     # Draw Iceland over.
     geoplot(grid=F,new=T)
     # Draw lines around Iceland, could also use geolines.
}}
% docclass is function
% Converted by Sd2Rd version 43268.
