Closed.curve <-
function(gogn, df = round(nrow(gogn)/2), n = 10)
{
	gogn$index <- c(1:nrow(gogn))
	tmpper <- c(1, nrow(gogn))
	assign("tmpper", tmpper,pos=1)#, frame = 1) changed for R ver.
	assign("df", df, pos=1)#frame = 1)
	x <- glm(lat ~ ps(index, df = df, period = tmpper), data = gogn)
	y <- glm(lon ~ ps(index, df = df, period = tmpper), data = gogn)
	r <- range(gogn$index)
	pred.frame <- data.frame(index = seq(r[1], r[2], length = nrow(gogn) *
		n))
	pred.frame$lat <- predict(x, pred.frame)
	pred.frame$lon <- predict(y, pred.frame)
	pred.frame <- pred.frame[, c("lat", "lon")]
	return(pred.frame)
}

