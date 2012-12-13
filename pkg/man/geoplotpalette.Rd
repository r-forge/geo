\name{colps}
\alias{colps}
\title{
  Sets certain color scheme on the terminal.  The function is only for R
and does not work in Splus.  
}
\description{
A colorscheme is set on the terminal with color 1 black and hue in hsb
model changing from 0.6666 to 0 (blue to red) in 154 steps going
through green and yellow.  Colors 159-169 are then a black and white
color scheme going from black to white.  The color.scheme can be
observed by the command litir(14) that writes a 14x14 matrix of colors
on the screen.  
}
\usage{
colps(...)
}
\arguments{
No arguments are required
No arguments are optional
}
\value{
No value returned.  
}
\section{Side Effects}{
The color scheme on the screen changes.  
by dev.off()
}
\details{
<!--explain details here-->
}
\section{References}{

}
\seealso{
hsv,palette,litir 
}
\examples{
\dontrun{X11(bg="white") # white background default transparent (in R)
geoplotpalette()

# in windows the graphics device can be opened with the command 
# X11 but it does not allow the argument.  The native command is
# so an equivalent command there would be 
windows(bg="white") # white background default transparent (in R)
geoplotpalette()
}}
\keyword{<!--Put one or more s-keyword tags here-->}
% docclass is function
% Converted by Sd2Rd version 43268.
