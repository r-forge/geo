\name{litir}
\alias{litir}

\title{Display palette in effect}

\description{
A grid of colors is plotted.
}

\usage{
litir(n)
}

\arguments{
  \item{n}{Number of columns and rows of colors to display}
}

\details{
Palette in effect gets repeated for \code{n} greater than \code{length(palette())}.
}

\value{
No value returned, plots an \code{n} by \code{n} grid of colors.
}

\note{
Simpler version of \code{\link{Rlitir}}, one of those should do, possibly with a name change.
}

\seealso{\code{\link{Rlitir}}}

\keyword{colors}
