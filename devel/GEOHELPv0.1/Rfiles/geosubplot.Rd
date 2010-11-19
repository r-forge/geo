\name{geosubplot}
\alias{geosubplot}
\title{
  Adds a plot to an existing plot initialized by geoplot.
}
\description{
Adds a plot, of  any  kind,  to  the  current  plot  at  a
     location specified by the user.
}
\usage{
geosubplot(fun, pos, size=c(2, 2), fill, fillcol, ...)
}
\arguments{
\item{fun}{
a call to the function to be subplotted.  Be sure to  call
       geoplot with new = T.
}
\item{pos}{
Position of the plot.  Should include \$lat  and  \$lon,  if
       length(\$lat)  =  1 that point will determine the middle of
       the subplot, if length(\$lat)=2 the points  will  determine
       oppesite corners of the subplots position.
}
\item{size}{
 The  width   and   the   height   of   the   plot   if
       length(pos\$lat)=1.  Default is 2,2.
}
\item{fill}{
If true the background of  the  subplot  area  will  be
       filled.  Default is F.
}
\item{fillcol}{
fill colour, the colour  of  the  background  of  the
       subplot area, default is 0 (usually white)
}
\item{\dots}{
The program accepts any parameter that the subplot program
       will accept.
}
}
\value{
 none
}
\section{Side Effects}{
 A plot added to the current plot.
}
\details{
The geo S-plus programs rely on the parameters  in  geopar
       for  plotting.   Every time geoplot is called a new set of
       geopar is made and the current  erased.  If  you  want  to
       change  the  parameters  in  the background plot after you
       call geosubplot you have to save the current geopar before
       calling geosubplot. When reassigning geopar, the command assign()
must be use, it does not suffice to use <- See examples.
}
\seealso{
\code{\link{subplot}},
\code{\link{geoplot}},
\code{\link{geopar}}.
}
\examples{
      #####################################################
      # Example 1                                         #
      #####################################################

      geoplot()
      pos<- list(lat = 63.5,lon=-11.75)
      geosubplot(geoplot(faeroes,pch=" ",country =faeroes,new=T)
                 ,pos,fill=T)
      # Plots the Faeroes on a plot with Iceland. Be sure to
      # use new = T if geoplot is called again.

      #####################################################
      # Example 2                                         #
      #####################################################

      geoplot()
      large.geopar <- geopar             # Parameters saved.
      pos <- list(lat=c(63,64),lon=c(-27,-24))
      geosubplot(geoplot(island, new=T,grid=F,type="l"),pos)
  
      geotext(65,-18,"subplot")           # Text on subplot.
      small.geopar <- geopar # Parameters for subplot saved.
      
      # geopar <- large.geopar       # Make big plot active.
      # Unless you are working directly with the .Data dir of the 
      # geolibrary this assignment will not work, must use:
      assign("geopar",large.geopar,where=0)

      geotext(65,-18,"Big plot")         # Text on big plot.

      # Another subplot.

      pos <- list(lat=c(63,64),lon=c(-17,-14))
      geosubplot(geoplot(island,new=T,grid=F,type="l"),pos,fill=T)
      # Another subplot drawn.
      geotext(65,-18,"subplot # 2")
       
      small.geopar.2 <- geopar # parameters for subplot # 2 saved.
      # geopar <- large.geopar # Big plot made active again
      # Same as above, instead use:
      assign("geopar",large.geopar,where=0)

      # See also similar example in geopar.
}
% docclass is function
% Converted by Sd2Rd version 43268.
