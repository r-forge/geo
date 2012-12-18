d2ir <-
function(lat, lon = NULL, useI = FALSE)
{
  if(is.null(lon)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon - 1e-06
  outside <- lat < 36 | lat >= 85.5 | lon <= -44 | lon > 68.5
  if(any(outside))
    warning("Positions outside of ICES statistical area")
  lat <- floor(lat * 2) - 71
  lat <- ifelse(lat < 10, paste("0", lat, sep = ""), lat)
  if(useI) 
    lettersUsed <- LETTERS[1:12]
  else
    lettersUsed <- LETTERS[c(1:8,10:13)]
  lon1 <- lettersUsed[(lon + 60) %/% 10]
  lon2 <- ifelse(lon1 == "A", floor(lon %% 4), floor(lon %% 10))
  ir <- paste(lat, lon1, lon2, sep = "")
  ir[outside] <- NA
  ir
}

