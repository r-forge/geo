\name{gridaxes}
\alias{gridaxes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gridaxes(limx, limy, scale, b0, xyratio, grid, col, reitur, smareitur, axratio, axlabels, b1, l1, projection, dlat, dlon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{limx}{ ~~Describe \code{limx} here~~ }
  \item{limy}{ ~~Describe \code{limy} here~~ }
  \item{scale}{ ~~Describe \code{scale} here~~ }
  \item{b0}{ ~~Describe \code{b0} here~~ }
  \item{xyratio}{ ~~Describe \code{xyratio} here~~ }
  \item{grid}{ ~~Describe \code{grid} here~~ }
  \item{col}{ ~~Describe \code{col} here~~ }
  \item{reitur}{ ~~Describe \code{reitur} here~~ }
  \item{smareitur}{ ~~Describe \code{smareitur} here~~ }
  \item{axratio}{ ~~Describe \code{axratio} here~~ }
  \item{axlabels}{ ~~Describe \code{axlabels} here~~ }
  \item{b1}{ ~~Describe \code{b1} here~~ }
  \item{l1}{ ~~Describe \code{l1} here~~ }
  \item{projection}{ ~~Describe \code{projection} here~~ }
  \item{dlat}{ ~~Describe \code{dlat} here~~ }
  \item{dlon}{ ~~Describe \code{dlon} here~~ }
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
function(limx, limy, scale, b0, xyratio, grid, col, reitur, smareitur, axratio,
	axlabels, b1, l1, projection, dlat, dlon)
{
        axlabels=F        # added to make geoaxis default in init() for R ver.
	o <- invProj(limx, limy, scale, b0, b1, l1, projection)
	r1 <- (limy[2] - limy[1])/(limx[2] - limx[1])
	# ratio
	nlon <- 30
	nlat <- round((nlon * r1)/xyratio) * 2
	if(dlat == 0 && dlon == 0) {
		if((o$lon[2] - o$lon[1]) > 40)
			dlon <- 10
		if((o$lon[2] - o$lon[1]) > 1)
			dlon <- 1/3
		if((o$lon[2] - o$lon[1]) > 3)
			dlon <- 1/2
		if((o$lon[2] - o$lon[1]) > 6)
			dlon <- 1
		if((o$lon[2] - o$lon[1]) > 10)
			dlon <- 2
		if((o$lon[2] - o$lon[1]) > 20)
			dlon <- 4
		if((o$lon[2] - o$lon[1]) <= 1)
			dlon <- 1/6
		if((o$lon[2] - o$lon[1]) < 0.4)
			dlon <- 1/12
		if((o$lon[2] - o$lon[1]) < 0.2)
			dlon <- 1/30
		if((o$lon[2] - o$lon[1]) < 0.1)
			dlon <- 1/60
		if((o$lon[2] - o$lon[1]) < 0.05)
			dlon <- 1/120
		dlat <- dlon/2
		if(reitur) {
			dlon <- 1
			dlat <- 0.5
		}
		if(smareitur) {
			dlon <- 0.5
			dlat <- 0.25
		}
	}
	if(dlat == 0 && dlon != 0)
		dlat <- dlon/2
	if(dlat != 0 && dlon == 0)
		dlon <- dlat * 2
	dlat <- dlat/axratio
	dlon <- dlon/axratio
	olo <- o$lon[1] - ((o$lon[1]/dlon) - floor(o$lon[1]/dlon)) * dlon
	ola <- o$lat[1] - ((o$lat[1]/dlat) - floor(o$lat[1]/dlat)) * dlat
	latgr <- ola + c(0:(nlat * 2)) * dlat
	latgr[latgr > 85] <- 85
	longr <- olo + c(0:(nlon * 2)) * dlon
	latgr <- latgr[(latgr <= o$lat[2]) & (latgr > o$lat[1])]
	#171
	longr <- longr[(longr <= o$lon[2]) & (longr > o$lon[1])]
	latgr2 <- c(o$lat[1], latgr, o$lat[2])
	longr2 <- c(o$lon[1], longr, o$lon[2])
	nlat <- length(latgr2)
	nlon <- length(longr2)
	latgr1 <- matrix(latgr2, nlat, nlon)
	longr1 <- t(matrix(longr2, nlon, nlat))
	# 	plot grid
	plotgr2 <- Proj(latgr1, longr1, scale, b0, b1, l1, projection)
	n <- ncol(plotgr2$x)
	n1 <- c(1:n)
	n1[1:n] <- NA
	# add NA for plot
	plx.lon <- rbind(plotgr2$x, n1)
	ply.lon <- rbind(plotgr2$y, n1)
	par(err = -1)
	if(grid)
		lines(plx.lon, ply.lon, col = col)
	# plot grid. 
	n <- nrow(plotgr2$x)
	n1 <- c(1:n)
	n1[1:n] <- NA
	# add NA for plot
	plx.lat <- rbind(t(plotgr2$x), n1)
	ply.lat <- rbind(t(plotgr2$y), n1)
	par(err = -1)
	if(grid)
		lines(plx.lat, ply.lat, col = col)
	# plot grid.
	# 	Plot axes
	latcha <- round((abs(latgr) - trunc(abs(latgr))) * 60, digits = 2)
	loncha <- round((abs(longr) - trunc(abs(longr))) * 60, digits = 2)
	indlat <- latcha == 60
	indlon <- loncha == 60
	latchar <- as.character(trunc(abs(latgr)) + indlat)
	lonchar <- as.character(trunc(abs(longr)) + indlon)
	latcha <- as.character(latcha - indlat * 60)
	loncha <- as.character(loncha - indlon * 60)
	latmin <- rep("'", length(latcha))
	lonmin <- rep("'", length(loncha))
	if(floor(dlat) == dlat) {
		ind <- c(1:length(latcha))
		ind <- ind[latcha == "0"]
		latcha[ind] <- " "
		latmin[ind] <- " "
	}
	else latcha[latcha == "0"] <- "00"
	if(floor(dlon) == dlon) {
		ind <- c(1:length(loncha))
		ind <- ind[loncha == "0"]
		loncha[ind] <- " "
		lonmin[ind] <- " "
	}
	else loncha[loncha == "0"] <- "00"
	latchar <- paste(latchar, "°", latcha, latmin, sep = "")
	lonchar <- paste(lonchar, "°", loncha, lonmin, sep = "")
	latchar <- c(" ", latchar, " ")
	lonchar <- c(" ", lonchar, " ")
	#	vect<-c(1:length(longr2)); vect[1:length(longr2)] <- o$y[1] 
	vect <- rep(60, length(longr2))
	# bretting 11-7
	plotgrlon <- Proj(vect, longr2, scale, b0, b1, l1, projection)
	vect <- c(1:length(latgr2))
	vect[1:length(latgr2)] <- o$x[1]
	plotgrlat <- mercator(latgr2, vect, scale, b0)
	par(adj = 0.5)
	if(axlabels) {
		if(grid) {
			# how the axes are plotted. 
                        axis(1, plotgrlon$x, lonchar, tick = F, col = col) # If grid.
                        axis(2, plotgrlat$y, latchar, tick = F, col = col)
		}
                else {
			axis(1, plotgrlon$x, tick = F, col = col) # If axlabels.
			axis(3, plotgrlon$x, F, tick = F, col = col)
			axis(2, plotgrlat$y, latchar, tick = F, col = col)
			axis(4, plotgrlat$y, F, tick = F, col = col)
			xgr <- Proj(latgr, longr, scale, b0, b1, l1, projection
				)
			plot.nogrid(o, xgr$x, xgr$y, col)
		}
	}
		# no axlabels
		if(grid) {
			# how the axes are plotted.
                        axis(1, plotgrlon$x, F, tick = F, col = col)
			axis(2, plotgrlat$y, F, tick = F, col = col)
		}
		else {
			xgr <- Proj(latgr, longr, scale, b0, b1, l1, projection)
			plot.nogrid(o, xgr$x, xgr$y, col)
		}

	return(list(dlon=dlon,dlat=dlat))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
