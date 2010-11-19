\name{SegmentWithArrow}
\alias{SegmentWithArrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot line segment with arrow at the end.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SegmentWithArrow(pos, angle = 15, size = 0.2, minrat = 1, col = "blue", lwd = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos}{
%%     ~~Describe \code{pos} here~~
}
  \item{angle}{
%%     ~~Describe \code{angle} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{minrat}{
%%     ~~Describe \code{minrat} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(pos,angle=15,size=0.2,minrat=1,col="blue",lwd=2){
  plt.size <- geopar$gpar$pin
  dist <- arcdist(pos$lat[1],pos$lon[1],pos$lat[2],pos$lon[2],scale="Miles")
  arrowsize <- diff(geopar$origin$lat)*size/geopar$gpar$pin[2]*60
  rat <- min(c(arrowsize/dist,0.5))
  tmp <- pos
  tmp[1,] <- tmp[2,]+rat*(tmp[1,]-tmp[2,])
  tmp <- Proj(tmp)
  dx <- diff(tmp$x)
  dy <- diff(tmp$y) 
  rat <- tan(angle*pi/180)
  tmp1 <- data.frame(x=tmp$x[c(1,2,1)],y=tmp$y[c(1,2,1)])
  tmp1$y[1] <- tmp1$y[1]-rat*dx
  tmp1$x[1] <- tmp1$x[1]+rat*dy
  tmp1$y[3] <- tmp1$y[3]+rat*dx
  tmp1$x[3] <- tmp1$x[3]-rat*dy
  tmp1 <- invProj(tmp1)
  if(lwd > 0) geolines(pos,col=col,lwd=lwd)
  geopolygon(tmp1,col=col) 
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
