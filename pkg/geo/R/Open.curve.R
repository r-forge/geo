Open.curve <-
function(gogn, df = nrow(gogn)/2, n = 10)
{
	gogn$index <- c(1:nrow(gogn))
 	assign("df", df,pos=1)#, frame = 1) # Changed for R ver, pos added.
        df <- round(df)
	x <- glm(lat ~ ns(index, df = df), data = gogn)
	y <- glm(lon ~ ns(index, df = df), data = gogn)
	r <- range(gogn$index)
	pred.frame <- data.frame(index = seq(r[1], r[2], length = nrow(gogn) *
		n))
	pred.frame$lat <- predict(x, pred.frame)
	pred.frame$lon <- predict(y, pred.frame)
	pred.frame <- pred.frame[, c("lat", "lon")]
	return(pred.frame)
}

