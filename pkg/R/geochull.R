geochull <-
function(lat, lon = NULL)
{
	if(is.null(lon)) {
		lon <- lat$lon
		lat <- lat$lat
	}
	x <- Proj(lat, lon)$x
	y <- Proj(lat, lon)$y
	id <- chull(x, y)
	id <- c(id, id[1])
	data.frame(invProj(x[id], y[id])[c("lat", "lon")])
}

