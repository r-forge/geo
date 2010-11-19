\name{invProj}
\alias{invProj}
\title{
  Performs the inverse Mercator or Lambert projection of data.
}
\description{
accepts data in spherical coordinates with center lattitude 0 and longitude 0 and performs an inverse Mercator, Lambert or no tranformation.
}
\usage{
invProj(x, y=NULL, scale=geopar$scale, b0=geopar$b0, b1=geopar$b1,
         l1=geopar$l1, projection=geopar$projection)
}
\arguments{
\item{x,y}{
The input data to be inversely projected, may be given as two vectors
or as list attributes (\$x and \$y).
}
\item{scale}{
The scale of the input date (m, km or miles), default is the scale defined in geopar (the scale defined when the plot is initialized).
}
\item{b0}{
if projection = Mercator b0 is the center of the  Mercator projection. If projection = Lambert b0, b1 are the lattitudes defining the Lambert projection. Default are the b0 and b1 defined in geopar.
}
\item{l1}{
The longitude defining the Lambert projection, default is the l1 defined in geopar.
}
\item{projection}{
The projection to be inversed, legal projections are "mercator", "Lambert" and "none". Default is the projection defined in geopar.
}
}
\value{
The function returns a list containing if projection = "none" \$x and \$y, if projection is mercator or Lambert it includes the projection (\$projection), the scale (\$scale), \$lat and \$lon and \$x and \$y.
}
\seealso{
\code{\link{invProj}},
\code{\link{geopar}},
\code{\link{geoplot}}.
}
% docclass is function
% Converted by Sd2Rd version 43268.
