cut.multipoly <-
function(x, xb, in.or.out = 0)
{
	ind <- x$x[is.na(x$x)]
	if(length(ind) == 0) {
		x2 <- findcut(x, xb, in.or.out)
	}
	else {
		x2 <- list(x = NA, y = NA)
		ind <- prepare.line(x$x)
		for(i in 1:ind$nlx) {
			x1 <- list(x = x$x[ind$lx1[i]:ind$lx2[i]], y = x$y[
				ind$lx1[i]:ind$lx2[i]])
			x1 <- findcut(x1, xb, in.or.out)
			x2$x <- c(x2$x, NA, x1$x)
			x2$y <- c(x2$y, NA, x1$y)
		}
		x2$x <- x2$x[ - c(1:2)]
		x2$y <- x2$y[ - c(1:2)]
	}
	return(x2)
}

