\name{pre2s}
\alias{pre2s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pre2s(skr, rownames = F, dots.in.text = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{skr}{ ~~Describe \code{skr} here~~ }
  \item{rownames}{ ~~Describe \code{rownames} here~~ }
  \item{dots.in.text}{ ~~Describe \code{dots.in.text} here~~ }
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
function(skr, rownames = F, dots.in.text = T)
{
        fields <- count.fields(skr, sep = "\t")
        nrec <- length(fields)
        if(nrec == 2)
                return(NULL)
        collab <- scan(file = skr, what = character(), sep = "\t",
                n = fields[1])
        outp <- read.table(skr, sep = "\t", skip = 2, as.is = T, row.names =
                NULL, na.strings = "")
        names(outp) <- collab
        if(rownames) {
                row.names(outp) <- outp[, 1]
                outp <- outp[, 2:ncol(outp)]
        }
        # change _ in names to .
        if(dots.in.text) names(outp) <- skipta.texta(names(outp))
        return(outp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
