srA <-
function (sr, scale = "Miles") 
{
  sapply(sr, function(x) geoarea(srPeri(x)))
}

