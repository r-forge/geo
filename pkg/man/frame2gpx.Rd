\name{frame2gpx}
\alias{frame2gpx}

\title{
Convert latlon data frame to gpx file.
}
\description{
Writes a gpx file of a dataframe with a call to \code{gpsbabel} in the shell
}
\usage{
frame2gpx(data, filename = "tmp.gpx", type = "rte")
}

\arguments{
  \item{data}{
Data frame with positions in columns \code{lat} and \code{lon}.
}
  \item{filename}{
Name of gpx-file, defaults to 'tmp.gpx'.
}
  \item{type}{
Type of gpx-file, one of \code{wpt} for waypoints, \code{rte}' for route or \code{trk} for track.
}
}
\note{
Requires \code{gpsbabel} installation working from the command line.
}

\seealso{
\url{gpsbabel.org}
}
\examples{
\dontrun{
# some positions
pos <- rPeri(323)
frame2gpx(pos)
system("more tmp.gpx")
system("rm tmp.gpx")
}
}

\keyword{manip}
