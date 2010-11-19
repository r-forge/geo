geoinside <-
function(data, reg, option = 1, col.names = c("lat", "lon"), na.rm = T, robust
	 = F)
{
	if(!is.data.frame(data)) {
		i <- match(col.names, names(data))
		data <- data.frame(data[[i[1]]], data[[i[2]]])
		names(data) <- col.names
	}
	i <- match(col.names, names(data))
	index <- rep(NA, nrow(data))
	j <- rep(T, nrow(data))
	tmp <- data
	if(na.rm) {
		j <- !is.na(data[, i[1]]) & !is.na(data[, i[2]])
		data <- data[j,  ]
	}
	i1 <- match(col.names, names(reg))
	regx <- reg[[i1[1]]]
	regy <- reg[[i1[2]]]
	n <- length(regx)
	k <- (regx[1] != regx[n] || regy[1] != regy[n]) && length(regx) != 2
	if(k && !is.na(k)) {
		regx <- c(regx, regx[1])
		regy <- c(regy, regy[1])
	}
	reg <- list(x = regx, y = regy)
	if(length(reg$x) == 2)
		reg <- list(x = c(reg$x[1], reg$x[2], reg$x[2], reg$x[1], reg$
			x[1]), y = c(reg$y[1], reg$y[1], reg$y[2], reg$y[2],
			reg$y[1]))
	data <- list(x = data[[i[1]]], y = data[[i[2]]])
	border <- adapt(reg$y, reg$x, projection = "none")
	inside <- rep(0, length(data$x))
	# Robust method using trigonometric functions.  
	if(robust) {
		a <- a1 <- rep(0, length(reg$x))
		inside <- .C("marghc",
			as.double(data$x),
			as.double(data$y),
			as.integer(length(data$y)),
			as.double(border$x),
			as.double(border$y),
			as.integer(length(border$y)),
			as.integer(border$lxv),
			as.integer(length(border$lxv)),
			as.integer(inside),
			as.double(a),
			as.double(a1))
		inside <- inside[[9]]
	}
	else {
		# Faster method.  
		tmpinside <- rep(0, length(border$lxv))
		inside <- .C("geomarghc",
			as.double(data$x),
			as.double(data$y),
			as.integer(length(data$y)),
			as.double(border$x),
			as.double(border$y),
			as.integer(border$lxv),
			as.integer(length(border$lxv)),
			as.integer(inside),
			as.integer(tmpinside))
		inside <- inside[[8]]
	}
	index[j] <- inside
	inside <- index
	ind <- c(1:length(inside))
	ind <- ind[inside > 0 & !is.na(inside)]
	if(option == 1) {
		tmp <- tmp[ind,  ]
		return(tmp)
	}
	else if(option == 2) {
		tmp <- tmp[ - ind,  ]
		return(tmp)
	}
	else if(option == 3)
		return(inside)
	else if(option == 4)
		return(1 - inside)
	else if(option == 5) {
		ind <- c(1:length(inside))
		ind <- ind[inside == 0]
		return(ind)
	}
	else if(option == 6) {
		ind <- c(1:length(inside))
		ind <- ind[inside != 0]
		return(ind)
	}
	else return(ind)
}

