drA <-
function (dr, dlat = 1, dlon = 2, scale = "km") 
{
  if(!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  A <- sapply(dr, function(x, dlat, dlon) 
    geoarea(drPeri(x, dlat = dlat, dlon = dlon)),
      dlat = dlat, dlon = dlon)
  if(scale == "nmi") A/1.852^2
    else A
}

