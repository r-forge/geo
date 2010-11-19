\name{inside.reg.bc1}
\alias{inside.reg.bc1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inside.reg.bc1(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ ~~Describe \code{data} here~~ }
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
function(data)
{
        if(nrow(data) > 1)
                tmpdata <- data[, c("lat", "lon")]
        else tmpdata <- as.data.frame(data[, c("lat", "lon")])
        tmpdata$area <- rep(0, nrow(tmpdata))
        i <- 1
        ind <- geoinside(tmpdata, reg = reg.bc[[i]], option = 0, robust = F)
        if(length(ind) > 0)
                tmpdata[ind, "area"] <- i
        i <- 2
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 3
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 4
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 5
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 6
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 7
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 8
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 9
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 10
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 11
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 12
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 13
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 14
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 15
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        i <- 16
        j <- tmpdata$area == 0
        j1 <- c(1:length(j))
        j1 <- j1[j == T]
        if(length(j1) > 0) {
                ind <- geoinside(tmpdata[j1,  ], reg = reg.bc[[i]], option = 0,
                        robust = F)
                if(length(ind) > 0)
                        tmpdata[j1[ind], "area"] <- i
        }
        data$area <- tmpdata$area
        return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
