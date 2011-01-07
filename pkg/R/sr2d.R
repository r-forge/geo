sr2d <-
function(ssquare)
{
        square <- floor(ssquare/10)
        ssquare <- ssquare - square * 10
        lat <- floor(square/100)
        lon <- (square - lat * 100) %% 50
        halfb <- (square - 100 * lat - lon)/100
        lon <-  - (lon + 0.5)
        lat <- lat + 60 + halfb + 0.25
        l1.lat <- c(0, 0.125, 0.125, -0.125, -0.125)
        l1.lon <- c(0, -0.25, 0.25, -0.25, 0.25)
        lat <- lat + l1.lat[ssquare + 1]
        lon <- lon + l1.lon[ssquare + 1]
        return(list(lat = lat, lon = lon))
}

