gbplot <-
function(depth, col, lty, lwd, depthlab, depthlabcex)
{
	if(missing(depthlabcex))
		depthlabcex <- 0.7
	if(missing(lwd))
		lwd <- rep(1, length(depth))
	if(missing(lty))
		lty <- rep(1, length(depth))
	if(missing(col))
		col <- rep(1, length(depth))
	if(length(col) < length(depth))
		col[(length(col) + 1):length(depth)] <- col[length(col)]
	if(length(lwd) < length(depth))
		lwd[(length(lwd) + 1):length(depth)] <- lwd[length(lwd)]
	if(length(lty) < length(depth))
		lty[(length(lty) + 1):length(depth)] <- lty[length(lty)]
	for(i in 1:length(depth)) {
		dypi <- depth[i]
		if(dypi %% 100 != 0 || dypi == 300 || dypi == 700) {
			print(paste(dypi, "m lína ekki til í GEBCO gögnum"))
			return(invisible())
		}
		if(dypi <= 1000 || dypi == 1200 || dypi == 1500 || dypi == 2000
			)
			txt <- paste("geolines(gbdypi.", dypi, 
				",col=col[i],lwd=lwd[i],lty=lty[i])", sep = "")
		else {
			j <- match(dypi, names(gbdypi))
			txt <- paste("geolines(gbdypi[[", j, 
				"]],col=col[i],lwd=lwd[i],lty=lty[i])", sep = 
				"")
		}
		eval(parse(text = txt))
		if(!missing(depthlab)) {
			k <- !is.na(match(depthloc$z, dypi))
			if(any(k))
				geotext(depthloc[k,  ], z = depthloc[k, "z"],
					cex = depthlabcex)
		}
	}
	return(invisible())
}

