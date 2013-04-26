geocontour <-
function (grd, z, nlevels = 10, levels = NULL, labcex = 1, triangles = TRUE, 
    reg = 0, fill = 1, colors = TRUE, col = 1, only.positive = FALSE, 
    maxcol = 155, cex = 0.7, save = FALSE, plotit = TRUE, label.location = 0, 
    lwd = 1, lty = 1, labels.only = FALSE, digits = 1, paint = FALSE, 
    set = NA, col.names = c("lon", "lat"), csi = NULL, drawlabels = FALSE)
{
    if (!is.null(csi)) 
        cex <- cex * csi/0.12
    if (!is.null(attributes(grd)$grid)) {
        z <- grd
        grd <- attributes(grd)$grid
    }
    limits <- NULL
    maxn <- 10000
    grd <- Set.grd.and.z(grd, z, NULL, set, col.names)
    z <- grd$z
    z <- z + rnorm(length(z)) * 1e-09
    grd <- grd$grd
    grd <- extract(grd, z, maxn, limits, col.names = col.names)
    z <- grd$z
    grd <- grd$grd1
    ind <- c(1:length(z))
    ind <- ind[is.na(z)]
    if (length(ind) > 0) {
        if (fill == 0) 
            z[ind] <- -99999
        if (fill == 1) 
            z[ind] <- 0
        if (fill == 2) 
            z[ind] <- mean(z)
    }
    lon <- grd[[col.names[1]]]
    lat <- grd[[col.names[2]]]
    if (only.positive) {
        ind <- c(1:length(z))
        ind <- ind[z < mean(z[z > 0])/1000 & z != -99999]
        z[ind] <- mean(z[z > 0])/1000
    }
    cond1 <- col.names[1] == "lon" && col.names[2] == "lat"
    cond2 <- col.names[1] == "x" && col.names[2] == "y" && geopar$projection == 
        "none"
    if (cond1 || cond2) {
        oldpar <- selectedpar()
        on.exit(par(oldpar))
        par(geopar$gpar)
        if (geopar$cont) 
            par(plt = geopar$contlines)
    }
    if (cex != 0) 
        par(cex = cex)
    nx <- length(lon)
    ny <- length(lat)
    lon1 <- matrix(lon, nx, ny)
    lat1 <- t(matrix(lat, ny, nx))
    if (!labels.only) {
      if (geopar$projection == "Mercator" && col.names[1] == 
          "lon") {
        z <- matrix(z, nrow = length(lon), ncol = length(lat))
        lon2 <- c(matrix(lon[1], length(lat), 1))
        lat2 <- c(matrix(lat[1], length(lon), 1))
        xlat <- Proj(lat, lon2, geopar$scale, geopar$b0, 
                     geopar$b1, geopar$l1, geopar$projection)
        xlon <- Proj(lat2, lon, geopar$scale, geopar$b0, 
                     geopar$b1, geopar$l1, geopar$projection)
      }
      else {
        z <- matrix(z, nrow = length(lon), ncol = length(lat))
        xlon <- list(x = lon)
        xlat <- list(y = lat)
      }
    }
    if (colors) {
      if (is.null(levels)) {
        if (nlevels == 0) 
          nlevels <- 10
        levels <- pretty(z, nlevels)
      }
      nlevels <- length(levels)
      if (length(lty) == length(levels) && length(levels) > 
          1) 
        linetypes <- TRUE
      else {linetypes <- FALSE;lty <- rep(lty,length(levels))}
      if (length(lwd) == length(levels) && length(levels) > 
          1) 
        linew <- TRUE
      else {linew <- FALSE;lwd <- rep(lwd,length(levels))}
      if (length(col) == 1) {
        if (length(lty) == length(levels)) 
          color <- rep(1, nlevels)
        else {
          mincol <- 2
          color <- c(1:nlevels)
          color <- round(2 + ((color - 1) * maxcol)/(nlevels))
        }
      }
      else color <- col
      if (!labels.only) {
        if (length(ind) > 1) 
          z[ind] <- NA
        if (geopar$projection == "Lambert") {
          lev <- contourLines(lon + 400, lat, z, levels = levels)
          for (i in 1:length(lev)) {
            j <- match(lev[[i]]$level,levels)
            if (linew) 
              lw <- lwd[j]
            else lw <- 0
            if (linetypes) 
              lt <- lty[j]
            else lt <- 0
            geolines(lev[[i]]$y, lev[[i]]$x - 400, col = color[j], 
                     lty = lt, lwd = lw)
          }
        }
        else {
          for (i in 1:nlevels) {
            lev <- contour(xlon$x, xlat$y, z, axes = FALSE, drawlabels=drawlabels,
                           levels = c(levels[i], levels[i]), add = TRUE, 
                           triangles = triangles, labcex = labcex, xlim = geopar$limx, 
                           ylim = geopar$limy, col = color[i], xlab = " ", 
                           ylab = " ", save = FALSE, plotit = TRUE,lwd=lwd[i],lty=lty[i])
          }
        }
      }
    }
    else {
      if (length(ind) > 1) 
        z[ind] <- NA
      if (geopar$projection == "Lambert") {
        if (length(levels) == 1) 
          lev <- contourLines(lon + 400, lat, z, nlevels = nlevels)
        else {
          lev <- contourLines(lon + 400, lat, z, axes = FALSE, levels = levels)
          for (i in 1:length(lev)) {
            geolines(lev[[i]]$y, lev[[i]]$x - 400, col = col)
          }
          
        }
      }
      else {
        if (length(levels) == 1) 
          lev <- contour(xlon$x, xlat$y, z, nlevels = nlevels,
                         triangles = triangles, labcex = labcex, add = TRUE, 
                         xlim = geopar$limx, ylim = geopar$limy, axes = FALSE, 
                         col = col, xlab = " ", ylab = " ", save = save, 
                         plotit = plotit ,drawlabels=drawlabels)
        else lev <- contour(xlon$x, xlat$y, z, axes = FALSE, 
                            levels = levels, triangles = triangles, add = TRUE, 
                            labcex = labcex, xlim = geopar$limx, ylim = geopar$limy, 
                            col = col, xlab = " ", ylab = " ", save = save, 
                            plotit = plotit, drawlabels=drawlabels)
      }
    }
    if (save && geopar$projection == "Mercator") {
      lev <- contourLines(xlon$x, xlat$y, z, levels = levels)
      tmpdata <- data.frame(lat=0,lon=0,level=-99)
      res <- NULL
      for (i in 1:length(lev)) {
        tmp <- invProj(lev[[i]])
        tmp <- data.frame(lat=tmp$lat,lon=tmp$lon)
        tmp$level <- rep(lev[[i]]$level, nrow(tmp))
        res <- rbind(res, tmp)
        res <- rbind(res,tmpdata)
      }
      i <- res$lat == 0
      if(any(i)) res$lat[i] <- res$lon[i] <- NA
      lev <- res
    }
    if (save && geopar$projection == "Lambert") {
      tmpdata <- data.frame(lat=0,lon=0,level=-99)
      res <- NULL
      for (i in 1:length(lev)) {
        tmp <- lev[[i]]
        tmp <- data.frame(lat=tmp$y,lon=tmp$x-400)
        tmp$level <- rep(lev[[i]]$level, nrow(tmp))
        res <- rbind(res, tmp)
        res <- rbind(res,tmpdata)
      }
      i <- res$lat == 0
      if(any(i)) res$lat[i] <- res$lon[i] <- NA
      lev <- res
    }

    if (geopar$projection == "Lambert") 
      par(geopar$gpar)
    if (length(label.location) == 1) 
      if (label.location == "locator") 
        label.location <- geolocator(n = 2)
    if (length(label.location) > 1) {
      label.location <- Proj(label.location, scale = geopar$scale, 
                             b0 = geopar$b0, b1 = geopar$b1, l1 = geopar$l1, projection = geopar$projection)
      if (geopar$projection == "none") 
        paint.window.x(label.location, border = TRUE)
      else paint.window(label.location, border = TRUE)
        labels_line(levels, digits, color, lty, xlim = label.location$x, 
                    ylim = label.location$y, linew)
    }
    if (geopar$cont && colors) {
      par(plt = geopar$contlab)
      par(new = TRUE)
      plot(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0), type = "l", 
           axes = FALSE, xlab = " ", ylab = " ")
      labels_line(levels, digits, color, lty, linew)
    }
    if (length(reg) > 1 && paint) {
      nx <- length(lon)
      ny <- length(lat)
      lon <- matrix(lon, nx, ny)
      lat <- t(matrix(lat, ny, nx))
      shadeborder(reg, lat, lon)
    }
    if (cond1 || cond2) {
      par(oldpar)
    }
    if (save) {
      return(invisible(lev))
    }
    else return(invisible())
  }

