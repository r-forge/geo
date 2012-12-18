mrA <-
function (mr, dlat = 5, dlon = 10, scale = "nmi") 
{
  if(!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  A <- sapply(mr, function(x, dlat, dlon) 
    geoarea(mrPeri(x, dlat = dlat, dlon = dlon)), 
      dlat = dlat, dlon = dlon)
  if(scale == "nmi") A/1.852^2
    else A
}

