\name{sqlcommand}
\alias{sqlcommand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sqlcommand(skipun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{skipun}{ ~~Describe \code{skipun} here~~ }
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
function(skipun)
{
        tmpskra <- tempfile("splus")
        tmpskipanaskra <- tempfile("splussql++skipun")
        on.exit(unlink(tmpskra))
        #on.error(unlink(tmpskra))
#        on.exit(unlink(tmpskipanaskra), add = T)
        #on.error(unlink(tmpskipanaskra),add=T)
        write(skipun, file = tmpskipanaskra)
        skipun <- paste("sql++ -p <", tmpskipanaskra, ">", tmpskra)
        system(skipun)
        x <- pre2s(tmpskra)
        names(x) <- skipta.texta(names(x))
        # Breyta _ í .
        return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
