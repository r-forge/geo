icelandrivers <- rivers

data <- c("island","geopar.std","greenland","bisland","eyjar","faeroes","icelandrivers","glaciers","gbdypi","gbdypi.100","gbdypi.200","gbdypi.500","gbdypi.400","gbdypi.800","gbdypi.1000","gbdypif.500","gbdypif.200","gbdypif.400","gepco500","depthloc","geopar.std","postcol")
data <- c(data,"nonsetpar","twohmiles")
# Vantar COUNT ??
          
functions <-c("adapt","adjust.grd","apply.shrink","apply.shrink.dataframe","arcdist","Arrow","bisland","bua","Closed.curve","colsymbol","combine.rt","COUNT","COUNTRY.DEFAULT","cut.box.1","cut.box.2","cut.multipoly","d2r","Elimcomp","extract","eyjar","faeroes","fill.matrix","fill.outside.border","fill.points","findcut","find.hnit","findline","fitspher.aut.1","gbdypi.100","gbplot","geoarea","geoaxis","geocontour","geocontour.fill","geoconvert","geoconvert.1","geoconvert.2","geocurve","geodefine","geodezoom","geoexpand","geogrid","geoidentify","geoinside","geolegend","geolines","geolines.with.arrows","geolocator","geopar","geopar.std","geoplot","geopoints","geopolygon","geo.Split.poly","geosubplot","geosymbols","geotext","geoworld","geozoom","giveborder","greenland","gridaxes","gridaxes.Lambert","gridpoints","init","inside","inside.reg.bc","inside.reg.bc1","invlambert","invmerc","invProj","island","join","join.data.frame","labels1","labels2","labels.line","Rlitir","colps","bwps","geoplotpalette","geoplotbwpalette","SegmentWithArrow","currentarrows","geointersect")

# 1 lína of löng
functions <- c(functions, c("lambert","last.warning","litir","mercator","nonsetpar","Open.curve","orthproj","paint.window","paint.window.x","pdist","pdistx","plot.nogrid","plot.reitnr","plvar","pointkriging","pre2s","prepare.line","Proj","ps","r2d","reitaplott","s2pre","selectedpar","selpos","Set.grd.and.z","setgrid","shadeborder","shading1","skipta.texta","sqlcommand","sr2d","subplot","variofit","variogram","SMB.std.background","pltgrid","locdist","labels.size","COUNT","geotows"))
functions <- functions[is.na(match(functions,data))]
          
package.skeleton(list=c(data,functions),name="geo")

                 
