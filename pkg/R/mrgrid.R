mrgrid <-
function (mr, dlat = 5, dlon = 10, fill = FALSE, ...) 
{
  mr <- lapply(mr, mrPeri, dlat = dlat, dlon = dlon)
  if(fill) {
    invisible(lapply(1:length(mr), function(i, mr, ...)
      geopolygon(mr[[i]], col = col[i], ...), mr = mr, ...))
  }
  else {
    invisible(lapply(1:length(mr), function(i, mr, ...)
      geolines(mr[[i]], col = col[i], ...), mr = mr, ...))
  }
}
