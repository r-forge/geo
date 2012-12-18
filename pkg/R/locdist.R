locdist <-
function(scale = "nmi", type = "p")
{
        lat <- geolocator(n = 2, type = type)
        x <- arcdist(lat$lat[1], lat$lon[1], lat$lat[2], lat$lon[2])
        if(scale == "km")
                x <- x * 1.852
        return(x)
}

