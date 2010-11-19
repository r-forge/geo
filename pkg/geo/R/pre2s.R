pre2s <-
function(skr, rownames = F, dots.in.text = T)
{
        fields <- count.fields(skr, sep = "\t")
        nrec <- length(fields)
        if(nrec == 2)
                return(NULL)
        collab <- scan(file = skr, what = character(), sep = "\t",
                n = fields[1])
        outp <- read.table(skr, sep = "\t", skip = 2, as.is = T, row.names =
                NULL, na.strings = "")
        names(outp) <- collab
        if(rownames) {
                row.names(outp) <- outp[, 1]
                outp <- outp[, 2:ncol(outp)]
        }
        # change _ in names to .
        if(dots.in.text) names(outp) <- skipta.texta(names(outp))
        return(outp)
}

