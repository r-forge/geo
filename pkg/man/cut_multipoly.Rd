\name{cut_multipoly}
\alias{cut_multipoly}

\title{Intersect or take complement (?) of polygons}

\description{
Intersect or take complement (?) of one or many polygons with a single
one.
}

\usage{
cut_multipoly(x, xb, in.or.out = 0)
}

\arguments{
  \item{x}{Polygon or polygons, seperated with NAs, hence 'multipoly'}
  \item{xb}{Polygon to intersect with/complement from}
  \item{in.or.out}{Whether to take the intersect (0) or complement of x
  in xb (1). Default 0}
}

\value{
List with compents:
  \item{x, y}{with coordinates of intersection or complement (?)}
}

\note{
Check use of \code{in.or.out=1}, when there are many polygons in
\code{x}, how is the complement with \code{xb} taken? Needs elaboration.
Possibly drop in.or.out from argument list and fix it at 0 in call to
findcut.
}

\seealso{
Called by \code{\link{geopolygon}}, calls \code{\link{findcut}} and
\code{\link{prepare.line}}
}

\keyword{manip}
