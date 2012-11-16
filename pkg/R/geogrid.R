geogrid <-
function (lat, lon = 0, col = 1, type = "l", lwd = 0, lty = 0, 
    pch = "+", nx = 5) 
{
    oldpar <- selectedpar()
    if (length(lon) == 1) {
        if (geopar$projection == "none") {
            lon <- lat$y
            lat <- lat$x
        }
        else {
            lon <- lat$lon
            lat <- lat$lat
        }
    }
    if (geopar$projection == "Lambert") 
        nx <- nx
    else nx <- 1
    if (geopar$projection != "none") {
        if (mean(lat, na.rm = T) > 1000) {
            lat <- geoconvert(lat)
            lon <- -geoconvert(lon)
        }
    }
    if (type == "l") {
        llon <- length(lon)
        llat <- length(lat)
        latgr <- t(matrix(lat, llat, llon))
        longr <- matrix(lon, llon, llat)
        latgr <- rbind(latgr, rep(NA, ncol(latgr)))
        longr <- rbind(longr, rep(NA, ncol(longr)))
        geolines(latgr, longr, 
          col = col, lwd = lwd, lty = lty, nx = nx)
        llon <- length(lon)
        llat <- length(lat)
        latgr <- matrix(lat, llat, llon)
        longr <- t(matrix(lon, llon, llat))
        latgr <- rbind(latgr, rep(NA, ncol(latgr)))
        longr <- rbind(longr, rep(NA, ncol(longr)))
        geolines(latgr, longr, 
          col = col, lwd = lwd, lty = lty, nx = nx)
    }
    else {
        llon <- length(lon)
        llat <- length(lat)
        latgr <- c(t(matrix(lat, llat, llon)))
        longr <- c(matrix(lon, llon, llat))
        geopoints(latgr, longr, pch = pch)
    }
    return(invisible())
    par(oldpar)
}
