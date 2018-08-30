normalizing <- function(x) {
  if (sum(x)==0) {
    return(x)
  } else {
    return(x/sum(x))
  }
}
