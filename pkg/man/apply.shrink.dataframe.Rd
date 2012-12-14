\name{apply.shrink.dataframe}
\alias{apply.shrink.dataframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
apply.shrink.dataframe(data, name.x, name.ind, FUNS = NULL, NA.rm = F, resp.name = NULL, full.data.frame = F, Set = NA, name.res, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{name.x}{ ~~Describe \code{name.x} here~~ }
  \item{name.ind}{ ~~Describe \code{name.ind} here~~ }
  \item{FUNS}{ ~~Describe \code{FUNS} here~~ }
  \item{NA.rm}{ ~~Describe \code{NA.rm} here~~ }
  \item{resp.name}{ ~~Describe \code{resp.name} here~~ }
  \item{full.data.frame}{ ~~Describe \code{full.data.frame} here~~ }
  \item{Set}{ ~~Describe \code{Set} here~~ }
  \item{name.res}{ ~~Describe \code{name.res} here~~ }
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

 ~Make other sections like Warning with section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data, name.x, name.ind, FUNS = NULL, NA.rm = F, resp.name = NULL,
	full.data.frame = F, Set = NA, name.res, ...)
{
	COUNT <- function(x)
	return(length(x))
	FUNS <- as.character(substitute(FUNS))
	if(!is.na(match(FUNS[1], "c")))
		FUNS <- FUNS[2:length(FUNS)]
	i <- match(name.ind, names(data))
	if(any(is.na(i))) {
		i1 <- c(1.:length(i))
		i1 <- i1[is.na(i)]
		stop(paste("Column", name.ind[i1], "does not exist"))
	}
	i <- match(name.x, c(names(data), "NR"))
	if(any(is.na(i))) {
		i1 <- c(1.:length(i))
		i1 <- i1[is.na(i)]
		stop(paste("Column", name.x[i1], "does not exist"))
	}
	data$NR <- rep(1., nrow(data))
	i <- match("", name.x)
	# Eyða NA gildum.  
	if(!is.na(i)) name.x[i] <- "NR"
	i <- rep(1., nrow(data))
	if(NA.rm) {
		k <- match(name.x, names(data))
		for(j in 1.:length(name.x)) {
			if(is.numeric(data[, k[j]])) {
				i <- i & !is.na(data[, k[j]])
			}
		}
		data <- data[i,  ]
	}
	if(length(name.x) > 1 && length(FUNS) == 1)
		FUNS <- rep(FUNS, length(name.x))
	if(length(name.x)== 1 & length(FUNS) > 1) name.x <- rep(name.x,length(FUNS))
        if(missing(name.res)) 
	   name.res <- paste(name.x, FUNS, sep = ".")
	name.res <- c(name.ind, name.res)
	indices <- list()
	for(i in 1:length(name.ind))
		indices[[i]] <- data[, name.ind[i]]
	if(full.data.frame) {
		x <- tapply(rep(1, nrow(data)), indices, sum)
		result <- expand.grid(dimnames(x))
		x <- c(x)
		j <- is.na(x)
		for(i in 1:length(FUNS)) {
			x <- c(tapply(data[, name.x[i]], indices, FUNS[i]))
			if(any(j))
				x[j] <- Set
			result <- cbind(result, x)
		}
	}
	else {
		for(i in 1:length(FUNS)) {
			x <- apply.shrink(data[, name.x[i]], indices, FUNS[
				i])
			if(i == 1)
				result <- x
			else result <- cbind(result, x[, ncol(x)])
		}
	}
	names(result) <- name.res
	return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
