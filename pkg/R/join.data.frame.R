join.data.frame <-
function(x, y)
{
        z <- data.frame(x, y)
        names(z) <- c(names(x), names(y))
        return(z)
}

