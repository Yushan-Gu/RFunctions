replaceAll <- function(v, r, new){
  replace(v, v %in% r, new)
}

replaceEach <- function(v, l1, l2){
  for (i in 1:length(l1)) {
    v <- replaceAll(v, l1[i], l2[i])
  }
  return(v)
}
