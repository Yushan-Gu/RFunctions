replaceAll <- function(v, r, new){
  replace(v, v %in% r, new)
}

replaceEach <- function(v, l1, l2){
  for (i in 1:length(l1)) {
    v <- replaceAll(v, l1[i], l2[i])
  }
  return(v)
}

orderBy <- function(table, col, dec = T){
  if(!dec){
    table[order(table[[col]]),]
  }else{
    table[rev(order(table[[col]])),]
  }
}

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

give.colnames <- function(Tab, nameslis){
  colnames(Tab) <- nameslis
  return(Tab)
}




