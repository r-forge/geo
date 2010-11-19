\name{apply.shrink}
\alias{apply.shrink}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
apply.shrink(X, INDICES, FUN = NULL, names, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ ~~Describe \code{X} here~~ }
  \item{INDICES}{ ~~Describe \code{INDICES} here~~ }
  \item{FUN}{ ~~Describe \code{FUN} here~~ }
  \item{names}{ ~~Describe \code{names} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(X, INDICES, FUN = NULL, names, ...)
{
	# GJ 9/94.
	# 'apply.shrink' is identical to 'tapply' (see tapply).
	# But it returns a data.frame were each 'index' represent a column
	# and an extra column for the result of evaluating FUN for the partation
	# on X given by the INDICES.
	if(missing(FUN)) stop(
			"No function to apply to data given (missing argument FUN)"
			)
	if(!is.list(INDICES))
		INDICES <- list(INDICES)
	len.data <- length(X)
	all.indices <- rep(0., len.data)
	for(i in rev(INDICES)) {
		# combine all indices to one
		if(length(i) != len.data) stop(
				"Data and all indices must have same length")
                i <- as.factor(i) 
#		i <- as.category(i)
		all.indices <- all.indices * length(levels(i)) + (as.vector(
			unclass(i)) - 1.)
	}
	# one-origin
	all.indices <- all.indices + 1.
	INDICES <- as.data.frame(INDICES)
	INDICES <- INDICES[match(sort(unique(all.indices)), all.indices, 
		nomatch = 0.),  ]
	if(is.character(FUN))
		FUN <- getFunction(FUN)
	else if(mode(FUN) != "function") {
		farg <- substitute(FUN)
		if(mode(farg) == "name")
			FUN <- getFunction(farg)
		else stop(paste("\"", farg, "\" is not a function", sep = ""))
	}
	X <- split(X, all.indices)
	X.apply <- lapply(X, FUN, ...)
	numb.FUN.value <- length(X.apply[[1.]])
	if(numb.FUN.value == 1.)
		X.apply <- data.frame(X = unlist(X.apply))
	else X.apply <- data.frame(matrix(unlist(X.apply), ncol = 
			numb.FUN.value, byrow = T, dimnames = list(NULL, names(
			X.apply[[1.]]))))
	X.apply <- cbind(INDICES, X.apply)
	if(!missing(names))
		names(X.apply) <- names
	return(X.apply)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
