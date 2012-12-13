\name{grid}
\alias{grid}
\title{
  Grid is a function that prepares a list to use with the  programs
pointkriging  and blockkriging.  The program calculates a grid of
points covering the range  of  the  datapoints.   Then  only  the
gridpoints that are inside specified region are used. Many of the
parameters for the program are the same as in geoplot.
}
\usage{
grid(lat, lon=0, type="p", pch="*", xlim=c(0, 0), ylim=c(0, 0),
     b0=65, r=1.1, country=island, xlab="default", ylab="default",
     option="cut", reg=0, dx=c(0, 0), nx=c(0, 0), grpkt=0, scale="km",
     find=F, new=F, grid=T, projection="Mercator", n=2500, b1=b0, nholes=0)
}
\arguments{
\item{lat}{
Latitude of points plotted. (or x coordinate)
}
\item{lon}{
Longitude of points plotted. ( or y coordinate)  If  lon=0
       then  lat  is  a  list containing lat\$lat and lat\$lon. (or
       \$x,\$y) Negative values mean western longitudes.
}
\item{type}{
Same as an argument with the same name  in  the  "plot"
       command   in  Splus.   Options  are  type="l"  for  lines,
       type="p" for points.  Default value is "p".
}
\item{pch}{
x limits of  drawing  (longitudinal  direction).   Program
       finds the default values from the range of the data
}
\item{xlim}{
 x limits of drawing (longitudinal direction).   Program
       finds the default values from the range of the data.
}
\item{ylim}{
 y limits of drawing.  Program finds the default  values
       from the range of the data.
}
\item{b0}{
Latitude for the mercator transform.
}
\item{r}{
Size of area that is plotted.  r = 1.0 means  exactly  the
       range of the datapoints, but r=1.5 means that the range is
       1.5 times the range of data.  Default value is 1.1.
}
\item{country}{
Country that is plotted same as in geoplot. Which map
       is used depends on the size of the area plotted.  If small
       part of the coast is seen bisland is used,  else  Iceland.
       Ilands  can be added later by : " geolines(eyjar). Default
       is iceland.
}
\item{xlab}{
X-label.  Default value is "Longitude".
}
\item{ylab}{
Y-label.  Default value is "Latitute".
}
\item{option}{
Can be either "cut" or "nocut".  If  "nocut"  the  plot
       always  fills the plotting area but if "cut" the plot does
       not fill it in the direction where the range  of  data  is
       minimum.   It  has  to  be  kept  in mind that the program
       always keeps the same scale vertically  and  horizontally.
       Default  value  is "cut".  Not effective when contourplots
       are plotted.
}
\item{reg}{
List with components reg\$lat  and  reg\$lon.   Only  points
       within  the region  are used.  Holes in area are marked by
       NA.  If reg=0 the region is given by the locator function.
}
\item{dx}{
Distance between points in the grid in  degrees  longitude
       and  latitude.   (longitude  first) Default value is 50x50
       grid over the range of interest.  At  65  degree  north  1
       degree latitude = 111.1km and 1 degree longitude = 47 km.
}
\item{nx}{
Number of grid points in x and y direction.  (lon,lat)
}
\item{grpkt}{
List with two components \$lat and \$lon.   Latitude  and
       longitude of the points in the matrix.  Default value is 0
       which means that the program selects the points  from  the
       range of data and values of dx.
}
\item{scale}{
"km" or miles.  Default value is "km".
}
\item{find}{
If F the program does not look  for  which  points  are
       inside  the  region,  else  it  does.  Default value is F.
       Find = F should be used  in  most  cases  except  all  the
       points  are  to  be used at once in kriging. Then reducing
       the number of points helps a lot.
}
\item{new}{
If new is F a new plot is made else  it  is  plotted  over
       what is on the screen.
}
\item{grid}{
If grid is F no meridians of longitude and paralells of
       latitude are not plotted.
}
\item{projection}{
"Mercator", "Lambert" or "none", default mercator.
}
\item{n}{
 Total number of points, divided by the program in x and  y
       direction  to get even spacing. Default is 2500.  The user
       can specify any of the parameters dx, grpkt, nx, n or none
       of them.
}
\item{b1}{
Second latitude in Lambert transform.  Default is the same
       as b0.
}
\item{nholes}{

}
}
\value{
 A list with the following component.        xgr  list with
       the  coordinates  of  the points that are           inside
       the border. Components \$lat and \$lon.        grpt latitude
       and   longitude   of   the   rows   and   columns  of  the
                 matrix    that    xgr    is    selected    from.
              reg  The   borders   of  the  region  of  interest.
              find If points inside the region were found or not.
       The  list  from  the  program  grid goes directly into the
       programs pointkriging and blockkriging.
}
\seealso{
\code{\link{geoplot}},
\code{\link{geogrid}},
\code{\link{pointkriging}},
\code{\link{variogram}}.
}
\examples{\dontrun{
       grlat <- c(1:20)*0.5+60
       grlon <- -10-c(1:20)
       xgr <- grid(lon=grlon,lat=grlat)

       # Here a plot is created, which shows our data as it would
       # be plotted with geoplot(). The user is prompted for GUI
       # input. Done as in geozoom. That is we define the area of our
       # grid by selecting any two points on the plot and the pushing
       # the middle button.
}}
% docclass is function
% Converted by Sd2Rd version 43268.
