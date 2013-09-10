geoplot <-
function(lat = NULL, lon = 0, type = "p", pch = "*", xlim = c(0, 0), 
  ylim = c(0, 0), b0 = 65, r = 1.05, country = "default", xlab = " ", 
  ylab = " ", option = "cut", grid = TRUE, new = FALSE, cont = FALSE,
  cex = 0.9,col = 1, lcont= c(0.13, 0.21), plotit = TRUE, 
  reitur = FALSE, smareitur = FALSE, reittext = FALSE, cexrt = 0.7, 
  csirt=NULL, axratio = 1, lwd = 0, lwd1 = 0, locator = FALSE, 
  axlabels = TRUE, projection = "Mercator", b1 = b0, dlat = 0, dlon = 0, 
  jitter = 0, zoom, csi = NULL, xaxdist = 0.2, yaxdist = 0.3)
{
  geopar <- getOption("geopar")
  if(!is.null(csirt)) cexrt <- cexrt*csirt/0.12
  if(!is.null(csi)) cex <- cex*csi/0.12
  if(!plotit) axlabels <- FALSE  # not plot axes if ther is no plot.  
  if(!missing(zoom)) {
    xlim <- geolocator(n = 2)
  }
	oldpar.1 <- par(no.readonly = TRUE)
 	# first version of old parameters
	command <- sys.call()
	if((oldpar.1$fig[2] - oldpar.1$fig[1]) <= 0.6 || (oldpar.1$fig[4] -
		oldpar.1$fig[3]) <= 0.6)
		multfig <- TRUE
	else multfig <- FALSE
	if(projection == "none") {
                if(is.list(xlim) &&  any(!is.na(match(c("x","y"),names(xlim))))){
                  ylim <- xlim$y
                  xlim <- xlim$x
		}
	}
	else {
                if(is.list(xlim) &&  any(!is.na(match(c("lat","lon"),names(xlim))))){
                  ylim <- xlim$lat
                  xlim <- xlim$lon
		}
	}
	if(is.null(lat) && xlim[2] == xlim[1] && ylim[2] == ylim[1] && !locator
		) {
		#std plot
		if(!multfig) par(fig = geopar.std$fig)
		if(!multfig)
			par(plt = geopar.std$plt)
		xlim <- geopar.std$xlim
		ylim <- geopar.std$ylim
		if(!multfig)
			par(mex = geopar.std$mex)
	}
	if(is.null(lat)) {
		lat <- c(65, 66)
		lon <- c(-28, -27)
		type <- "n"
	}
	oldpar <- selectedpar()
	if(locator & missing(zoom)) {
		limits <- geolocator(n = 2)
		if(geopar$projection == "none") {
			xlim <- limits$x
			ylim <- limits$y
		}
		else {
			xlim <- limits$lon
			ylim <- limits$lat
		}
	}
	xlim <- sort(xlim)
	ylim <- c(ylim)
	if(projection == "none") {
		if(length(country) == 1)
			if(country == "default")
				country <- "none"
	}
	else {
		if(length(country) == 1)
                  if(country == "default")
                    eval(parse(text = paste("country <- ",COUNTRY.DEFAULT)))
	}
	init(lat, lon = lon, type = type, pch = pch, xlim = xlim, ylim = ylim,
		b0 = b0, r = r, country = country, xlab = xlab, ylab = ylab,
		option = option, grid = grid, new = new, cont = cont, cex = cex,
		col = col, lcont = lcont, plotit = plotit, reitur = reitur,
		smareitur = smareitur, reittext = reittext, axratio = axratio,
		lwd = lwd, axlabels = axlabels, oldpar = oldpar, projection = 
		projection, b1 = b1, dlat = dlat, dlon = dlon, command = 
		command, jitter = jitter, xaxdist = xaxdist, yaxdist = yaxdist)
        oldpar.1 <- Elimcomp(oldpar.1)
	par(oldpar.1)
#        par(new = TRUE)
	if(reittext)
		plot_reitnr(cexrt, lwd = lwd)
	# number of squares
	if(length(country) > 1 && plotit) geolines(country, col = col, lwd = 
			lwd1)
	# plot country
	return(invisible())
}

