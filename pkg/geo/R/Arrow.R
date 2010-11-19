Arrow <-
function (pos, angle = 15, col = 2)
{
    pos <- Proj(pos)
    dx <- -diff(pos$x)
    dy <- -diff(pos$y)
    d <- sqrt(dy * dy + dx * dx)
    d1 <- d * tan((angle * pi)/180)
    p1y <- pos$y[1] + d1/d * dx
    p1x <- pos$x[1] - d1/d * dy
    p2y <- pos$y[1] - d1/d * dx
    p2x <- pos$x[1] + d1/d * dy
    d <- data.frame(y = c(pos$y[2], p1y, p2y, pos$y[2]), x = c(pos$x[2],
        p1x, p2x, pos$x[2]))
    d <- invProj(d)
    geopolygon(d,col=col)
    return(invisible(data.frame(lat = d$lat, lon = d$lon)))
}

