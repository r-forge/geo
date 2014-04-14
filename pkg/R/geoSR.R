geoSR <- function(sr, z, levels, grid=FALSE, ...)
{
  if(!is.null(names(levels)))
  {
    opal <- palette(c("black", names(levels)))
    on.exit(palette(opal))
  }

  geoplot(grid=grid, ...)
  invisible(capture.output(reitaplott(reitur=sr, smareitur=NULL, z=z, levels=levels, colors=seq(levels), density=0)))
  geopolygon(island)
  geolines(island)
}
