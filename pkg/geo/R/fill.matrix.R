fill.matrix <-
function(outcome, x, rownr, dalknr)
{
        ind <- nrow(outcome) * (dalknr - 1) + rownr
        outcome[ind] <- x
        return(outcome)
}

