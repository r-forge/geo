skipta.texta <-
function(txt, char = "_", replchar = ".")
{
        backsl <- "\\"
        tmpfile1 <- tempfile("splusskipt")
        tmpfile2 <- tempfile("splusskipt")
        tmpskipanaskra <- tempfile("splusskipun")
        on.exit(unlink(tmpfile1))
        on.exit(unlink(tmpfile2))
        on.exit(unlink(tmpskipanaskra))
        txt <- paste(txt, collapse = "\n")
        write(txt, file = tmpfile1)
        skipun <- paste("sed 's/", backsl, char, "/", backsl, replchar, "/g' <",                tmpfile1, ">", tmpfile2, sep = "")
        write(skipun, file = tmpskipanaskra)
        system(paste("chmod u+x", tmpskipanaskra))
        system(tmpskipanaskra)
        txt <- scan(tmpfile2, what = character(), sep = "\t")
        return(txt)
        print(skipun)
}

