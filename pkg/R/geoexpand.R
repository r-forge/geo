geoexpand <-
function(grid)
{
	if(is.null(grid$lat)) {
		ny <- length(grid$y)
		nx <- length(grid$x)
		y <- t(matrix(grid$y, ny, nx))
		x <- matrix(grid$x, nx, ny)
		return(data.frame(y = c(y), x = c(x)))
	}
	else {
		nlat <- length(grid$lat)
		nlon <- length(grid$lon)
		lat <- t(matrix(grid$lat, nlat, nlon))
		lon <- matrix(grid$lon, nlon, nlat)
		return(data.frame(lat = c(lat), lon = c(lon)))
	}
}

