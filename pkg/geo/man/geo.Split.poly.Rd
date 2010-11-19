\name{geo.Split.poly}
\alias{geo.Split.poly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
geo.Split.poly(data)
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
	while(is.na(data[nrow(data), "lat"])) data <- data[ - nrow(data),  ]
	while(is.na(data[1, "lat"])) data <- data[-1,  ]
	n <- nrow(data)
	if(any(is.na(data$lat))) {
		tmp <- list()
		i <- 1:nrow(data)
		i <- i[is.na(data$lat)]
		i1 <- c(1, i + 1)
		i2 <- c(i - 1, nrow(data))
		for(i in 1:length(i1)) {
			tmp[[i]] <- data[i1[i]:i2[i],  ]
			n <- nrow(tmp[[i]])
			if(tmp[[i]]$lat[1] != tmp[[i]]$lat[n] || tmp[[i]]$
				lon[1] != tmp[[i]]$lon[n])
				tmp[[i]] <- rbind(tmp[[i]], tmp[[i]][1,  ])
			if(nrow(tmp[[i]]) < 4) {
				cat("minimum of 3 points needed to define area"
					)
				print(tmp[[i]][1:(nrow(tmp[[i]]) - 1)])
			}
		}
	}
	else {
		if(data$lat[1] != data$lat[n] || data$lon[1] != data$lon[n])
			data <- rbind(data, data[1,  ])
		tmp <- list(c1 = data)
	}
	return(tmp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
