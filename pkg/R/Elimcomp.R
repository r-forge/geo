Elimcomp <-
function(parlist){
  txt <- names(parlist)
  txt <- txt[is.na(match(txt,nonsetpar))]
  res <- list()
  for(i in txt) res[[as.character(i)]] <- parlist[[as.character(i)]]
  return(res) 
}

