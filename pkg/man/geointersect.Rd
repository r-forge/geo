\name{geointersect}
\alias{geointersect}

\title{
Find intersection of 2 polygons
}
\description{
Find intersection of 2 polygons
}
\usage{
geointersect(data, border, in.or.out)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{border}{
%%     ~~Describe \code{border} here~~
}
  \item{in.or.out}{
%%     ~~Describe \code{in.or.out} here~~
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
\code{\link{findcut}}, \code{\link{invProj}}, \code{\link{Proj}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data, border, in.or.out)
{
	tmp <- invProj(findcut(Proj(data), Proj(border), in.or.out))
	return(data.frame(lat = tmp$lat, lon = tmp$lon))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
