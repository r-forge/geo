geozoom <-
function()
{
	geopar <- getOption("geopar")
	if(as.character(geopar$command[length(geopar$command)]) != "123")
		com <- c(geopar$command, zoom = 123)
	else com <- geopar$command
	eval(com)
}

