apply.shrink.dataframe <-
function(data, name.x, name.ind, FUNS = NULL, NA.rm = FALSE, resp.name = NULL,
	full.data.frame = FALSE, Set = NA, name.res, ...)
{
	COUNT <- function(x)
	return(length(x))
	FUNS <- as.character(substitute(FUNS))
	if(!is.na(match(FUNS[1], "c")))
		FUNS <- FUNS[2:length(FUNS)]
	i <- match(name.ind, names(data))
	if(any(is.na(i))) {
		i1 <- c(1.:length(i))
		i1 <- i1[is.na(i)]
		stop(paste("Column", name.ind[i1], "does not exist"))
	}
	i <- match(name.x, c(names(data), "NR"))
	if(any(is.na(i))) {
		i1 <- c(1.:length(i))
		i1 <- i1[is.na(i)]
		stop(paste("Column", name.x[i1], "does not exist"))
	}
	data$NR <- rep(1., nrow(data))
	i <- match("", name.x)
	# Remove NA values
	if(!is.na(i)) name.x[i] <- "NR"
	i <- rep(1., nrow(data))
	if(NA.rm) {
		k <- match(name.x, names(data))
		for(j in 1.:length(name.x)) {
			if(is.numeric(data[, k[j]])) {
				i <- i & !is.na(data[, k[j]])
			}
		}
		data <- data[i,  ]
	}
	if(length(name.x) > 1 && length(FUNS) == 1)
		FUNS <- rep(FUNS, length(name.x))
	if(length(name.x)== 1 & length(FUNS) > 1) name.x <- rep(name.x,length(FUNS))
        if(missing(name.res)) 
	   name.res <- paste(name.x, FUNS, sep = ".")
	name.res <- c(name.ind, name.res)
	indices <- list()
	for(i in 1:length(name.ind))
		indices[[i]] <- data[, name.ind[i]]
	if(full.data.frame) {
		x <- tapply(rep(1, nrow(data)), indices, sum)
		result <- expand.grid(dimnames(x))
		x <- c(x)
		j <- is.na(x)
		for(i in 1:length(FUNS)) {
			x <- c(tapply(data[, name.x[i]], indices, FUNS[i]))
			if(any(j))
				x[j] <- Set
			result <- cbind(result, x)
		}
	}
	else {
		for(i in 1:length(FUNS)) {
			x <- apply.shrink(data[, name.x[i]], indices, FUNS[
				i])
			if(i == 1)
				result <- x
			else result <- cbind(result, x[, ncol(x)])
		}
	}
	names(result) <- name.res
	return(result)
}

