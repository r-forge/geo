\name{inside.reg.bc}
\alias{inside.reg.bc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inside.reg.bc(data, only.sv.1to10 = F, ignore.latlon = F, ignore.area0 = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{only.sv.1to10}{ ~~Describe \code{only.sv.1to10} here~~ }
  \item{ignore.latlon}{ ~~Describe \code{ignore.latlon} here~~ }
  \item{ignore.area0}{ ~~Describe \code{ignore.area0} here~~ }
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
function(data, only.sv.1to10 = F, ignore.latlon = F, ignore.area0 = T)
{
        if(is.na(match("lat", names(data)))) {
                data$lat <- data$lon <- rep(NA, nrow(data))
        }
        if(!is.na(match("area", names(data)))) {
                print("warning column area exists")
                return(invisible(data))
        }
        n <- nrow(data)
        data$area <- rep(NA, nrow(data))
        index <- !is.na(data$lat) & !is.na(data$lon)
        index1 <- c(1:length(index))
        index1 <- index1[index]
        if(ignore.latlon)
                index1 <- NULL
        if(length(index1) > 0) {
                area <- inside.reg.bc1(data[index1, c("lat", "lon")])$area
                data$area[index1] <- area
        }
        if(is.na(match("reitur", names(data))))
                return(data)
        if(ignore.latlon)
                index1 <- c(1:nrow(data))
        else {
                index <- (is.na(data$lat) | is.na(data$lon)) & !is.na(data$
                        reitur)
                if(ignore.area0)
                        index <- index | (data$area == 0 & !is.na(data$area))
                index1 <- c(1:length(index))
                index1 <- index1[index]
        }
        if(length(index1) > 0) {
                reitdata <- data[index1,  ]
                if(only.sv.1to10) {
                        reitdata$area <- Reitur2Svaedi1to10(reitdata$reitur)
                }
                else {
                        if(is.na(match("smareitur", names(reitdata))))
                                reitdata$smareitur <- rep(0, nrow(reitdata))
                        else reitdata$smareitur[is.na(reitdata$smareitur)] <-
                                        0
                        tmp <- data.frame(sr2d(reitdata$reitur * 10 + reitdata$
                                smareitur))
                        ind <- c(1:nrow(tmp))
                        ind <- ind[!is.na(tmp$lat)]
                        reitdata$area[ind] <- inside.reg.bc1(tmp[ind,  ])$
                                area
                }
                data$area[index1] <- reitdata$area
        }
        return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
