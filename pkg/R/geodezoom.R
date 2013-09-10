geodezoom <-
function()
{
	geopar <- getOption("geopar")
	if(as.character(geopar$command[length(geopar$command)]) == "123")
		com <- geopar$command[1:(length(geopar$command) - 1)]
	else com <- geopar$command
	eval(com)
}

