"Reitur2Svaedi1to10"<-
function(reitur, smareitur)
{
        if(missing(smareitur))
                smareitur <- rep(0, length(reitur))
        a <- rep(0, length(reitur))
        i <- match(reitur, Totalreitir$reitur)  # reitir allir innan sama sv.
        i1 <- c(1:length(i))
        i1 <- i1[!is.na(i)]
        i <- i[!is.na(i)]
        a[i1] <- Totalreitir$area[i]
        i <- match(reitur, Dypisreitir$reitur)  # reitir utan og innan 500m
        i1 <- c(1:length(i))
        i1 <- i1[!is.na(i)]
        i <- i[!is.na(i)]
        if(length(i1) > 0)
                a[i1] <- Dypisreitir[i, "<500"]
        i <- match(reitur, c(373, 324))
        i1 <- c(1:length(i))
        i1 <- i1[!is.na(i)]
        if(length(i1) > 0)
                a[i1] <- 1
        i <- match(reitur, c(373, 324)) # reitir bæði á 1 og 10
        i1 <- c(1:length(i))
        i1 <- i1[!is.na(i)]
        if(length(i1) > 0) a[i1] <- 1   # meirihluti í sv. 1.
        i <- match(reitur, c(721, 722, 723))    # reitir bæði á 2 og 3.
        i1 <- c(1:length(i))
        i1 <- i1[!is.na(i)]
        if(length(i1) > 0)
                a[i1] <- 2
        i <- match(reitur, c(Dypisreitir$reitur, Totalreitir$reitur, 323, 324,
                721, 722, 723))
        i1 <- c(1:length(i))
        i1 <- i1[is.na(i)]
        if(length(i1) > 0)
                tmp <- inside.reg.bc1(r2d(reitur[i1] * 100 + smareitur[i1]))
        a[i1] <- tmp$area
        return(a)
}

