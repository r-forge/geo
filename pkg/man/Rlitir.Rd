\name{Rlitir}
\alias{Rlitir}

\title{Display colors.}

\description{
A grid of colors is plotted.
}

\usage{
Rlitir(n, col)
}

\arguments{
  \item{n}{Number of columns and rows of colors to display}
  \item{col}{A vector of colors}
}

\value{
No value returned, plots an \code{n} by \code{n} grid of colors.
}

\seealso{\code{\link{colorRampPalette}}}

\examples{
# simple, perhaps not so useful application with default palette:

Rlitir(12, 1:144)

# Define a palette with some colors:

ramp <- colorRampPalette(c("khaki1", "gold", "orange", 
  "darkorange2", "red", "darkred", "black"))

# number of columns and rows to display

n <- 10

Rlitir(n, ramp(n^2))
}

\keyword{color}
