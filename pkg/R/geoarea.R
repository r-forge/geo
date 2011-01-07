geoarea <-
function(data, Projection = "Lambert", old.method = F, ngrdpts = 2000, robust
	 = T)
{
	area <- 0
	data <- geo.Split.poly(data)
	if(old.method) {
		for(i in 1:length(data))
			area <- area + geoarea.old(data[[i]], ngrdpts, robust)
	}
	else {
		area <- 0
		for(i in 1:length(data)) {
			if(Projection == "Lambert")
				data[[i]] <- lambert(data[[i]]$lat, data[[i]]$
					lon, mean(data[[i]]$lat), mean(data[[
					i]]$lon), mean(data[[i]]$lat))
			else data[[i]] <- mercator(data[[i]]$lat, data[[i]]$
					lon, b0 = mean(data[[i]]$lat))
			data[[i]] <- data.frame(x = data[[i]]$x, y = data[[
				i]]$y)
			n <- nrow(data[[i]])
			area <- area + abs(sum(data[[i]]$x[1:(n - 1)] * data[[
				i]]$y[2:n] - data[[i]]$x[2:n] * data[[i]]$
				y[1:(n - 1)], na.rm = T)/2)
		}
	}
	return(area)
}

