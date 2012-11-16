rA <-
function (r, scale = "Miles") 
{
  sapply(r, function(x) geoarea(rPeri(x)))
}

