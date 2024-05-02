asName <- function(name)
{
  N <- gsub(pattern = "[^A-Za-z0-9_]", 
                 replacement = '.', 
                 name)
  sapply(N, function(l)if_else(grepl("[0-9]",substr(l,1,2)),
                               paste0("X",l), l)) %>% return()
  }

asNameExp <- function(text){
  Name <- ""
  index <- gregexpr("`", text)[[1]] %>% as.vector()
  Name <- paste0(Name,substr(text, 1,index[1]))
  for (i in (1:(length(index)/2))*2-1) {
    Name <- paste0(Name, substr(text,index[i]+1,index[i+1]-1) %>% asName(),"`")
    if(index[i+1]!=str_length(text)){
      Name <- paste0(Name, substr(text,index[i+1]+1,index[i+2]-1),"`")
    }
  }
  Name %>% return()
}

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

give.rownames <- function(Tab, nameslis){
  rownames(Tab) <- nameslis
  return(Tab)
}



