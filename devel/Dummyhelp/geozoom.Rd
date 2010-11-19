\name{geozoom}
\alias{geozoom}
\title{
  Zoom into plots.
}
\description{
Zoom into current geoplot, redraws  current  geoplot  with
       area defined by user.  The current plot can be restored by
       using geodezoom.
}
\usage{
geozoom()
}
\value{
none.
}
\section{Side Effects}{
The last  call  to  geoplot  is  recalled  with  different
       borders for x and y.
}
\seealso{
\code{\link{geoplot}},
\code{\link{geodezoom}}.
}
\examples{
    geoplot()
    geozoom()
    # Click with mouse as when placing legend i.e. first place the
    # mouse where the upper left corner is supposed to be and click
    # once, then move to where the lower right corner is supposed to
    # be and also press once.
    geodezoom()
    # Return to previous plot, here geoplot().
}
% docclass is function
% Converted by Sd2Rd version 43268.
