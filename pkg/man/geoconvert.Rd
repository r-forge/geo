\name{geoconvert}
\alias{geoconvert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
geoconvert(data, inverse = F, col.names = c("lat", "lon"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{inverse}{ ~~Describe \code{inverse} here~~ }
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
function(data, inverse = F, col.names = c("lat", "lon"))
{
	if(!inverse) {
		if(is.data.frame(data)) {
			if(any(is.na(match(col.names, names(data))))) {
				cat(paste("Columns", colnames, "do not exist"))
				return(invisible())
			}
			data[, col.names[1]] <- geoconvert.1(data[, col.names[
				1]])
			data[, col.names[2]] <- geoconvert.1(data[, col.names[
				2]])
		}
		else data <- geoconvert.1(data)
	}
	else {
		# Convert to write out. 
		if(is.data.frame(data)) {
			if(any(is.na(match(col.names, names(data))))) {
				cat(paste("Columns", colnames, "do not exist"))
				return(invisible())
			}
			data[, col.names[1]] <- geoconvert.2(data[, col.names[
				1]])
			data[, col.names[2]] <- geoconvert.2(data[, col.names[
				2]])
		}
		else data <- geoconvert.2(data)
	}
	return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
