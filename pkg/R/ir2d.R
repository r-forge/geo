ir2d <-
function(ir, useI = FALSE)
{
  lat <- substring(ir, 1, 2)
  lat <- as.numeric(lat)
  lat <- (lat +71)/2 + 0.25
  lon1 <- substring(ir, 3, 3)
  lon1 <- toupper(lon1)
  lon1 <- match(lon1, LETTERS)
  if(!useI)
    lon1 <- ifelse(lon1 > 8, lon1 - 1, lon1)
  lon1 <- lon1 - 2
  lon2 <- substring(ir, 4)
  lon2 <- as.numeric(lon2)
  ifelse(lon1 < 0,
    lon <- -44 + lon2 + 0.5,
    lon <- -40 + 10*lon1 + lon2 + 0.5)
  data.frame(lat = lat, lon = lon)
}
