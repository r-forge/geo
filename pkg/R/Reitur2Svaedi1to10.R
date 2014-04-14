"Reitur2Svaedi1to10"<-
function(reitur, smareitur, Totalreitir, Dypisreitir)
{
        if(missing(smareitur))
                smareitur <- rep(0, length(reitur))
        a <- rep(0, length(reitur))
        i <- match(reitur, Totalreitir$reitur)  # all rectangles within same Bormicon area
        i1 <- c(1:length(i))
        i1 <- i1[!is.na(i)]
        i <- i[!is.na(i)]
        a[i1] <- Totalreitir$area[i]
        i <- match(reitur, Dypisreitir$reitur)  # rectangles outside and inside 500 m
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
        i <- match(reitur, c(373, 324))  # rectangles in areas 1 and 10
        i1 <- c(1:length(i))
        i1 <- i1[!is.na(i)]
        if(length(i1) > 0) a[i1] <- 1  # mainly in area 1
        i <- match(reitur, c(721, 722, 723))  # rectangles straddling areas 2 and 3
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

