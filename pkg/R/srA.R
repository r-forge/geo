srA <-
function (sr, scale = "nmi") 
{
  A <- sapply(sr, function(x) geoarea(srPeri(x)))
  if(scale == "nmi") {
    A/1.852^2
  }
  else {
    A
  }
}

