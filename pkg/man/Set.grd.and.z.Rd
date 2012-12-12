\name{Set.grd.and.z}
\alias{Set.grd.and.z}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Set.grd.and.z(grd, z, mask, set = NA, col.names = c("lon", "lat"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grd}{ ~~Describe \code{grd} here~~ }
  \item{z}{ ~~Describe \code{z} here~~ }
  \item{mask}{ ~~Describe \code{mask} here~~ }
  \item{set}{ ~~Describe \code{set} here~~ }
  \item{col.names}{ ~~Describe \code{col.names} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(grd, z, mask, set = NA, col.names = c("lon", "lat"))
{
	# z is a name of a column in the dataframe grd.
	if(is.data.frame(grd) && is.character(z)) z <- grd[, z]
	if(is.data.frame(grd) && nrow(grd) == length(z)) {
		i1 <- match(col.names[1], names(grd))
		i2 <- match(col.names[2], names(grd))
		xgr <- sort(unique(grd[, i1]))
		ygr <- sort(unique(grd[, i2]))
		xgr.1 <- c(matrix(xgr, length(xgr), length(ygr)))
		ygr.1 <- c(t(matrix(ygr, length(ygr), length(xgr))))
		xgr.data <- data.frame(x = xgr.1, y = ygr.1)
		names(xgr.data) <- col.names
		xgr.data$z <- rep(set, nrow(xgr.data))
		index <- paste(xgr.data[, 1], xgr.data[, 2], sep = "-")
		index1 <- paste(grd[, i1], grd[, i2], sep = "-")
		j <- match(index1, index)
		xgr.data$z[j] <- z
		grd1 <- list(xgr, ygr)
		names(grd1) <- col.names
		return(list(grd = grd1, z = xgr.data$z))
	}
	# grd is a list like returned by pointkriging
	if(is.list(grd) && !is.data.frame(grd)) {
		i1 <- match(col.names[1], names(grd))
		i2 <- match(col.names[2], names(grd))
		xgr <- grd[[i1]]
		ygr <- grd[[i2]]
		if(length(xgr) * length(ygr) != length(z)) {
			cat("Incorrect length on z")
			return(invisible())
		}
		xgr.1 <- c(matrix(xgr, length(xgr), length(ygr)))
		ygr.1 <- c(t(matrix(ygr, length(ygr), length(xgr))))
		xgr.data <- data.frame(x = xgr.1, y = ygr.1)
		names(xgr.data) <- col.names
		xgr.data$z <- z
		grd1 <- list(xgr, ygr)
		names(grd1) <- col.names
		return(list(grd = grd1, z = xgr.data$z))
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
