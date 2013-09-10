\name{geopar}
\alias{geopar}
\title{
  Parameters for the geo plot functions.
}
\description{
The \pkg{geo} functions rely on the parameters in
\code{options("geopar")} for plotting. Every time \code{geoplot} is
called a new set of geopar is made and the current erased. If you want
to change the parameters in the background plot after you call
geosubplot you have to save the current geopar before calling
geosubplot. When reassigning geopar, the command
\code{options(geopar=list(...))} must be used.

When using \code{geosubplot} it is important to save these
parameters before the subplot is plotted if one wants to make changes to
the large plot afterwards.
}
\examples{
\dontrun{geoplot(type="l")
large.geopar <- geopar                          # Parameters saved.

pos <- list(lat=c(63,64),lon=c(-27,-24))    
geosubplot(geoplot(island, new=TRUE,type="l"),pos) # Plot subplot at pos.
geopolygon(island,col=3)                        # Fill with color.      
geotext(65,-18,"subplot",col=150)               # Text on subplot.
small.geopar <- geopar                          # Parameters for subplot saved.

geopar <- large.geopar                          # Make big plot active.
geopolygon(island,col=150)                      # Fill with color.
geotext(65,-18.5,"Big plot",csi=8,col=3)        # Text on big plot.

pos <- list(lat=c(63,64),lon=c(-15,-12))
geosubplot(geoplot(island,new=T,type="l"),pos)  # Another subplot drawn.
geopolygon(island,col=3)                        # Fill with color.
geotext(65,-18,"subplot # 2",col=150)           # Text on subplot #2.
small.geopar.2 <- geopar                        # Parameters for subplot saved.

# This must put in a script and sourced together to work like meant to here.
# If done in command line, assign() must be used for changing geopar.
}}
% docclass is data
% Converted by Sd2Rd version 43268.
