\name{pre2s}
\alias{pre2s}

\title{Read prelude files}

\description{
Read data files in prelude format, which has 2 line headers and column names seperated from data with dashes.
}

\usage{
pre2s(skr, rownames = F, dots.in.text = T)
}

\arguments{
  \item{skr}{Prelude file name}
  \item{rownames}{Should first column be used as row names? Default FALSE}
  \item{dots.in.text}{Should underscores in column names be replaced with "."? Default TRUE}
}

\value{
A data frame (\code{\link{data.frame}}) containing a representation of the data in the file.
}

\note{
Call to \code{skipta.texta} could be replaced with a call to \code{\link{chartr}} (as in ROracleUI sql).
}

\seealso{
Calls \code{\link{skipta.texta}}
}

\keyword{ ~kwd1 }
