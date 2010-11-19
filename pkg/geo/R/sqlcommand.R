sqlcommand <-
function(skipun)
{
        tmpskra <- tempfile("splus")
        tmpskipanaskra <- tempfile("splussql++skipun")
        on.exit(unlink(tmpskra))
        #on.error(unlink(tmpskra))
#        on.exit(unlink(tmpskipanaskra), add = T)
        #on.error(unlink(tmpskipanaskra),add=T)
        write(skipun, file = tmpskipanaskra)
        skipun <- paste("sql++ -p <", tmpskipanaskra, ">", tmpskra)
        system(skipun)
        x <- pre2s(tmpskra)
        names(x) <- skipta.texta(names(x))
        # Breyta _ í .
        return(x)
}

