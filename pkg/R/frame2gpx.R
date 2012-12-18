frame2gpx <-
function(data, filename = "tmp.gpx", type = "rte") {
  if (!is.data.frame(data)) stop("'data' not a dataframe")
  container <- tempfile("gpx")
  on.exit(unlink(container))
  colid <- match(c("lat", "lon"), names(data))
  data <- data[ , colid]
  data <- data.frame(data, 0:(nrow(data)-1))
  write.table(data, file = container,
    row.names = FALSE, col.names = FALSE, sep = ",")
  switch(type,
    wpt = system(paste("gpsbabel -i csv -f", container, 
      "-x transform -o gpx -F", filename)),
    rte = system(paste("gpsbabel -i csv -f", container, 
      "-x transform,rte=wpt,del -o gpx -F", filename)),
    trk = system(paste("gpsbabel -i csv -f", container, 
      "-x transform,trk=wpt,del -o gpx -F", filename))
  )
}

