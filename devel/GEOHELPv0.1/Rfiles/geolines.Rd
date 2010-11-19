\name{geolines}
\alias{geolines}
\title{
  Add lines to current plot initialized by geoplot.
}
\description{
Add lines to a plot initialized by geoplot. Data is stored as lat, lon
or x,y. Lists are assumed to have the components \$x and \$y if projection in geoplot was "none", else \$lat,\$lon. The program transforms the data as specified in geoplot. Similar to the Splus function lies.
}
\usage{
geolines(lat, lon=0, col=1, lwd=0, lty=0, nx=1)
}
\arguments{
\item{lat}{
Latitude of data. ( or x coordinate)
}
\item{lon}{
Longitude of data. ( or y coordinate) Negative values mean western longitudes. Default value is zero. If lon is zero then the data is stored as lat\$lat and lat\$lon. (or lat\$x and lat\$y)
}
\item{col}{
Colour number used for plotting the lines, default value is 1.
}
\item{lwd}{
Line width. Default is to use the width set when the program was called.
}
\item{lty}{
Line type. Default is to use the width set when the program was called. See Splus manuals for numbers corresponding to different linetypes and linewidths.
}
\item{nx}{
Parameter only used with Lambert transform when lines in lat,lon are curves in x,y. If nx > 1, nx-1 points are put between each two datapoints in lat, lon before projection is done. For example :
<s-example>
              geolines(c(66,66),c(-30,-10),nx=50)
</s-example>
plots a line onto the 66 degree latitude from -30 to -10. The line is curved because it is made of 50 segments.
}
}
\value{
No values returned.
}
\section{Side Effects}{
The projection is stored in geopar\$projection and parameters for the transform in geopar\$b0, geopar\$b1 and geopar\$l1.
}
\seealso{
\code{\link{geoplot}},
\code{\link{geopolygon}},
\code{\link{geopoints}},
\code{\link{geotext}},
\code{\link{geosymbols}},
\code{\link{geocontour.fill}},
\code{\link{geogrid}},
\code{\link{geocontour}}.
}
\examples{
       geolines(island)                      # plot iceland.
       geolines(island$lat,island$lon,col=1) # same.

       #######################################################

       geoplot(xlim=c(0,-50),ylim=c(60,75),projection="Lambert")
       # Set up a Lambert plot.       

       geolines(c(66,66),c(-30,-10),nx=50,col=155,lwd=2)
       # Draw a line with colour 155 and width 2.

       geopolygon(island)
       geolines(island,col=3,lwd=3)
       geolines(eyjar,col=40)
       geolines(faeroes,col=40)
       geolines(greenland,col=3,lwd=3)
       geolines(janmayen,col=40)
       # Plot some more countries using geolines.
}
% docclass is function
% Converted by Sd2Rd version 43268.
