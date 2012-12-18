srA <-
function (sr, scale = "nmi") 
{
  if(!(scale == "nmi" | scale == "km")) 
    stop("Unit square nautical miles or kilometers only")
  A <- sapply(sr, function(x) geoarea(srPeri(x)))
  if(scale == "nmi") {
    A/1.852^2
  }
  else {
    A
  }
}

