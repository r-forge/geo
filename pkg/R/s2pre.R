s2pre <-
function(data, file = "splus.pre", na.replace = "")
{
	# data       :matrix or data.frame.
	# na.replace :a character to replace NA with.
	#
	# VALUE      :a prelude file, named "Splus.pre" by default.
	if(is.data.frame(data)) data <- as.matrix.data.frame(data)
	data[is.na(data) | data == "NA"] <- na.replace
	col.names <- dimnames(data)[[2]]
	if(is.null(col.names) || length(col.names) == 0)
		col.names <- paste("dalkur", 1:ncol(data), sep = "")
	row.names <- dimnames(data)[[1]]
	if(!is.null(row.names) && length(row.names) > 0) {
		col.names <- c("linu_nofn", col.names)
		data <- cbind(row.names, data)
	}
	n.of.col <- length(col.names)
	# Write out rownames:
	cat(col.names, sep = c(rep("\t", n.of.col - 1), "\n"), file = file)
	strika.lina <- rep("", n.of.col)
	for(i in 1:n.of.col)
		strika.lina[i] <- paste(rep("-", nchar(col.names[i])), collapse
			 = "")
	# Write out the ------ line:
	cat(strika.lina, sep = c(rep("\t", n.of.col - 1), "\n"), file = file,
		append = T)
	# Write out the data:
	cat(t(data), sep = c(rep("\t", n.of.col - 1), "\n"), file = file, 
		append = T)
	return(invisible(NULL))
}

