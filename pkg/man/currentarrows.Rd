\name{currentarrows}
\alias{currentarrows}

\title{
Plot arrows showing the size and direction of currents
}
\description{
Plot arrows showing the size and direction of currents
}
\usage{
currentarrows(data, maxsize = 0.5, maxn, col = "blue", lwd = 2, minrat = 0.5, arrowsize = 0.2, center = T)
}

\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{maxsize}{
%%     ~~Describe \code{maxsize} here~~
}
  \item{maxn}{
%%     ~~Describe \code{maxn} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{minrat}{
%%     ~~Describe \code{minrat} here~~
}
  \item{arrowsize}{
%%     ~~Describe \code{arrowsize} here~~
}
  \item{center}{
%%     ~~Describe \code{center} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}



\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data,maxsize=0.5,maxn,col="blue",lwd=2,minrat=0.5,arrowsize=0.2,center=T){
  res <- list()
  xsizerat <- geopar$gpar$pin[1]/diff(geopar$origin$lon)
  ysizerat <- geopar$gpar$pin[2]/diff(geopar$origin$lat)
  data$rat <- cos(data$lat*pi/180) 
  if(missing(maxn)) 
    maxn <- max(data$current)
  
  tmp <- data.frame(lat=c(1,1),lon=c(1,1)) 
  for(i in 1:nrow(data)) {
    tmp[1,] <- data[i,c("lat","lon")]
    tmp[2,"lon"] <- tmp[1,"lon"]+maxsize*data$current[i]/maxn*cos(data$angle[i]*pi/180)/data$rat[i]/ysizerat
    tmp[2,"lat"] <- tmp[1,"lat"]+maxsize*data$current[i]/maxn*sin(data$angle[i]*pi/180)/ysizerat
    if(center){ #center the arrow, else start
      dlat <- diff(tmp$lat)
      dlon <- diff(tmp$lon)
      tmp$lat <- tmp$lat -dlat/2
      tmp$lon <- tmp$lon -dlon/2
    }
    res[[i]] <- tmp
    SegmentWithArrow(tmp,lwd=lwd,size=arrowsize,col=col,minrat=minrat)
  }
  return(invisible(res)) 
  }
}

\keyword{ ~kwd1 }
