geoworld <-
function(regions = ".", exact = FALSE, boundary = TRUE, fill = FALSE, color = 1, lwd = 1, 
	lty = 1, plot = TRUE, type = "l", pch = ".", database = "world", 
	return.data = FALSE, outside=FALSE, allowed.size = 80000)
{
  resolution <- 0 #1
  interior <- FALSE
  r <- 1.2
  doproj <- FALSE
 if(fill)
    interior <- TRUE
  if(geopar$projection == "Lambert") {
                                        # complicated borders in lat, lon
    p1 <- list(x = c(geopar$limx[1], mean(geopar$limx), geopar$limx[
                 1], geopar$limx[2]), y = c(geopar$limy[1], geopar$limy[
                                        2], geopar$limy[2], geopar$limy[2]))
    limits <- invProj(p1$x, p1$y, geopar$scale, geopar$b0, geopar$
                      b1, geopar$l1, geopar$projection)
    xlim <- c(limits$lon[3], limits$lon[4])
    ylim <- c(limits$lat[1], limits$lat[2])
  }
  else {
    limits <- invProj(geopar$limx, geopar$limy, geopar$scale, 
                      geopar$b0, geopar$b1, geopar$l1, geopar$projection)
    xlim <- c(limits$lon[1], limits$lon[2])
    ylim <- c(limits$lat[1], limits$lat[2])
  }
  xlim <- mean(xlim) + r * (xlim - mean(xlim))	# add
  ylim <- mean(ylim) + r * (ylim - mean(ylim))	# to get everything
                                        # parameter checks
                                        # turn the region names into a list of polygon numbers
  gon <- maps:::mapname(database, regions, exact)
  n <- length(gon)
  if(n == 0) stop("nothing to draw: no recognized region names")	
                                        # turn the polygon numbers into a list of polyline numbers
  line <- maps:::mapgetg(database, gon, fill, c(-1000000, 1000000), c(-1000000, 
                                                               1000000))
  if(length(line$number) == 0) stop(
             "nothing to draw: all regions out of bounds")	
                                        # turn the polyline numbers into x and y coordinates
  if(fill)
    coord <- maps:::mapgetl(database, line$number, c(-1000000, 1000000), c(
                                                                    -1000000, 1000000))
  else {
    l <- abs(line$number)
    if(boundary && interior)
      l <- unique(l)
    else if(boundary)
      l <- l[!match(l, l[duplicated(l)], FALSE)]
    else l <- l[duplicated(l)]
    coord <- maps:::mapgetl(database, l, xlim, ylim)
    if(length(coord) == 0)
      stop("all data out of bounds")
  }
  if(doproj) {
    coord <- Proj(coord$y, coord$x, geopar$scale, geopar$b0, geopar$
                  b1, geopar$l1, geopar$projection)
    coord$error <- FALSE
  }
                                        # for filled regions, turn NA breaks at polylines into
                                        # NA breaks at polygons, deleting polygons for which
                                        # there is a corresponding NA color
  if(fill) {
    gonsize <- line$size
    color <- rep(color, length = length(gonsize))
    keep <- !is.na(color)
    coord[c("x", "y")] <- makepoly(coord, gonsize, keep)
    color <- color[keep]
  }
  if(return.data) return(data.frame(lat = coord$y, lon = coord$x))	
                                        # do the plotting, if requested
  data <- data.frame(lat = coord$y, lon = coord$x)
  if(plot){
    if(fill) geopolygon(data, col = color, allowed.size = allowed.size)
    if(!fill) geolines(data, col = color, lwd = lwd, lty = lty)
  }
  return(invisible())
}

