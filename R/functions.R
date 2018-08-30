#' normalize numbers such that they sum to 1
normalizing <- function(x) {
  if (sum(x)==0) {
    return(x)
  } else {
    return(x/sum(x))
  }
}
