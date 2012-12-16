\name{colps}
\alias{colps}
\title{
  Open a postscript device with the color scheme given by
geoplotpalette.  
}
\description{
The function starts a postscript device and the arguments are the
same as the arguments to postscript (height, width, file, bg etc).  
}
\usage{
colps(...)
}
\arguments{
No arguments are required
\item{...}{The arguments to postscript are optional. See help postscript.}
}
\value{
No value returned.  
}
\section{Side Effects}{
A graphics device is opened (often a file).  It must be closed again
by dev.off()
}
\details{
<!--explain details here-->
}
\section{References}{

}
\seealso{
postscript, geoplotpalette,litir
}
\examples{
\dontrun{
colps(file="map1.ps",height=6,width=5) 
geoplot(xlim=c(-28,-10),ylim=c(64,69))
geosymbols(data,z=data$value,circles=0.2,sqrt=T)
geopolygon(island,col="white")# paint white over the symbols 
geolines(island) # that are inside the country.  (island) 
dev.off() 

# same example in a different way.  
colps(file="map1.ps",height=6,width=5,bg="white") 
geoplot(xlim=c(-28,-10),ylim=c(64,69))
geosymbols(data,z=data$value,circles=0.2,sqrt=T)
geopolygon(island,col=0)#col 0 is now white
geolines(island) # was transparent earlier
dev.off() 
}}
\keyword{<!--Put one or more s-keyword tags here-->}
% docclass is function
% Converted by Sd2Rd version 43268.
