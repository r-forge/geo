\name{geoidentify}
\alias{geoidentify}
\title{
  Identifies points on plots using lat and lon coordinates.
}
\description{
 Works the same way as identify except that it also accepts
       coordinates  as  lat and lon.  Identifies points on a plot
       identified by the user.
}
\usage{
geoidentify(lat, lon=NULL, labels=1, n=0, plot=T, 
            atpen=T, offset=0.5, col=1, csi=0.12)
}
\arguments{
\item{lat,lon}{
Coordinates of points.  The coordinates can be  given
       by  two  vectors  or  a  matrix containin vectors \$lat and
       \$lon.
}
\item{labels}{
Vector giving  labels  for  each  of  the  points.   If
       supplied,  this  must have the same length as lat and lon.
       As a default the vector indece number of the  points  will
       be used.
}
\item{n}{
Maximum number of points to be identified.
}
\item{plot}{
If true, geoidentify plots the  labels  if  the  points
       identified.
}
\item{atpen}{
If true, plotted identification is relative to  locator
       position when the point is identified; otherwise, plotting
       is relative to the identified lat,lon value. This  can  be
       useful when points are crowded.  Default is true.
}
\item{offset}{
Identification is  plotted  as  a  text  string,  moved
       offset  charecter from the point.  If the locator was left
       (right) of the nearest point, the label will be offset  to
       the left (right) of the point.
}
\item{col}{
The color of the labels.
}
\item{csi}{
Character size, the size of the label characters.
}
}
\value{
 Indeces (in lat and lon) corresponding to  the  identified
       points.
}
\section{Side Effects}{
Labels are placed on the current plot if plot is true.
}
\details{
Observations that have missing values in either lat  or
       lon are
              treated as if they were not given.

              When using the X11  driver  under  the   X   Window
       System,  a
              point  is  identified  by  positioning  the  cursor
       over the
              point and pressing the  left   button.    To   exit
       identify
              press  the  middle  button  (both  buttons on a two
       button
              mouse)  while  the  cursor  is  in  the    graphics
       window.   The
              same   procedure   is   used  under  the   suntools
       driver.  This
              function may also be used with the "tek" drivers.

              Some devices that do not allow  interaction  prompt
       you  for
              an x,y pair.

              The nearest  point  to  the  locator  position   is
       identified,
              but must be at most 0.5 inches away.   In  case  of
       ties, the
              earliest point is identified.
}
\seealso{
\code{\link{identify}},
\code{\link{geolocator}},
\code{\link{geotext}}.
}
\examples{
\dontrun{       geoidentify(stations,labels=stations$temp)
       # plots the temperature in the closest measuring point.

       geoidentify(stations,atpen=F) 
       # plots the indece number of the station closest to
       # where pointed at the stations position.
}}
% docclass is function
% Converted by Sd2Rd version 43268.
