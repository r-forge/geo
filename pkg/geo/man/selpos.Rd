\name{selpos}
\alias{selpos}
\title{
  Geographical point selection.
}
\description{
Select geographical data points by arbitrary criterion.
}
\usage{
selpos(lat, lon=NULL, ind)
}
\arguments{
\item{lat}{
Latitude of points or list containing lat,lon.
}
\item{ind}{
Selection criterion.
}
\item{lon}{
Longitude of data points. If missing, this must be
               part of the lat argument.
}
}
\value{
Returns list  with  elements  lat,lon  which  satisfy  the
       criterion.
}
\details{
The normal way of working with  geographical  data  is  to
       store  positions as a list with names lat and lon. This is
       easier for most applications, except selection of subsets,
       where  it  is essential to access individual elements. The
       purpose of this routine is merely to  ease  the  selection
       process.
}
\seealso{
\code{\link{geoplot}},
}
\examples{
             subs<-selpos(pos,,z>6)# Select positions where z>6

       The Function is trivially defined as
       function(lat, lon = NULL, ind)
       {
                      if(is.null(lon)) {
                           lon <- lat$lon
                           lat <- lat$lat
                      }
                      lat <- lat[ind]
                      lon <- lon[ind]
                      return(lat, lon)
       }
}
% docclass is function
% Converted by Sd2Rd version 43268.
