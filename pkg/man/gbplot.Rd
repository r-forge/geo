\name{gbplot}
\alias{gbplot}
\title{
  GEBCO plot. Plots equidepth lines.
}
\description{
Plots lines of equal depths using a database from GEBCO.
}
\usage{
gbplot(depth, col, lty, lwd, depthlab, depthlabcsi)
}
\arguments{
\item{depth}{
A vector of the depths which  we  want  equidept  lines
       plotted.
}
\item{col}{
The colour of the lines, if the col vector is shorter than
       the  depth  vector  it  is repeated.  Default is all lines
       black.
}
\item{lty}{
Linetype, if the lty vector  is  shorter  than  the  depth
       vector it is repeated.  Default is all lines have linetype
       1.
}
\item{lwd}{
Linewidth, if the lwd vector is  shorter  than  the  depth
       vector it is repeated. Default is all lines have linewidth
       1.
}
\item{depthlab}{
A boolean variable determening whether labels  should
       be printed on equidepth lines, default is false.
}
\item{depthlabcsi}{
The size of depthlabels.
}
}
\value{
 None
}
\section{Side Effects}{
 Plots equidepth lines on current plot.
}
\seealso{
\code{\link{geoplot}},
\code{\link{geolines}}.
}
\examples{
   geoplot()   # Set up plot.
   
   gbplot(c(100,500,1000),depthlab=T,depthlabcex=0.2)
   # Plot depthlines for 100,500,1000,1500 m, showing the
   # depth on the line.   
}
% docclass is function
% Converted by Sd2Rd version 43268.
