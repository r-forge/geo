\name{plvar}
\alias{plvar}
\title{
  Plots a variogram estimated by variofit.  
}
\description{
Plot a variogram and a spherical model fit to it.
}
\usage{
plvar(vagram, n=4, fit=T, type="p")
}
\arguments{

\item{vagram}{
 List from the  program  variogram  with  the  following components.           
 <s-example>        
          \$range:   range of data.
          \$nugget:  nugget effect.             
          \$sill:    sill.
          \$vario:   z values of the variogram.
          \$xh:      mean distance for each class.

</s-example>
}
\item{n}{
The plot covers n times the range.  Default value  is  all
       the data.
}
\item{fit}{

}
\item{type}{

}
}
\value{
No values returned.
}
\seealso{
\code{\link{pointkriging}},
\code{\link{grid}}.
}
% docclass is function
% Converted by Sd2Rd version 43268.
