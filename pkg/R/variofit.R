variofit <-
function(vagram, model = 1, option = 2, interactivt = F, sill = 0)
{
	if(model == 1 && !interactivt && option < 5) {
		if(sill == 0 && length(vagram$variance) > 0)
			vagram$variance <- sill
		vgr <- fitspher.aut.1(vagram, option, sill)
		if(vgr$error == 1)
			return()
		return(list(rang1 = vgr$rang1, sill = vgr$sill, nugget = vgr$
			nugget, dist = vagram$dist, vario = vagram$vario, 
			number = vagram$number))
	}
	if(interactivt) {
		k <- 1
		ld <- floor(length(vagram$dist)/1.2)
		sill <- rang1 <- nugget <- rep(0, 10)
		ans <- "y"
		col <- 10
		plvar(vagram, fit = F)
		#		cat(" What type of model : , Gaussian, spherical ")
		while(ans == "y" || ans == "Y") {
			cat(" Give sill, range and nugget  in this order : \n")
			x <- locator(n = 3)
			sill[k] <- x$y[1]
			rang1[k] <- x$x[2]
			nugget[k] <- x$y[3]
			txt0 <- paste(" k = ", as.character(k))
			txt1 <- paste(" sill =", as.character(round(sill[k],
				digits = 2)))
			txt2 <- paste("range = ", as.character(round(rang1[
				k], digits = 2)))
			txt3 <- paste("nugget = ", as.character(round(nugget[
				k], digits = 2)))
			print(txt1)
			print(txt2)
			print(txt3)
			lines(vagram$dist, spherical(rang1[k], sill[k], nugget[
				k], vagram$dist), col = col)
			text(vagram$dist[ld], sill[k], as.character(k), col = 
				col)
			col <- col + 10
			cat(" \n Try again y/n  : ")
			ans <- readline()
			k <- k + 1
			if(k == 10)
				break()
		}
		nm <- 0
		cat("Give the number of the best model, default the last one:")
		nm <- scan(n = 1)
		if(length(nm) == 0)
			nm <- k - 1
		# default.  
		rang1 <- rang1[nm]
		sill <- sill[nm]
		nugget <- nugget[nm]
	}
	return(list(rang1 = rang1, sill = sill, nugget = nugget, dist = vagram$
		dist, vario = vagram$vario, number = vagram$number))
}

