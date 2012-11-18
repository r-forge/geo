rA <-
function (r, scale = "nmi") 
{
  A <- sapply(r, function(x) geoarea(rPeri(x)))
  if(scale == "nmi") {
    A/1.852^2
  }
  else {
    A
  }
}
