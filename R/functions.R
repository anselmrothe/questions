
#' normalize numbers such that they sum to 1
#' @export
normalizing <- function(x) {
  if (sum(x)==0) {
    return(x)
  } else {
    return(x/sum(x))
  }
}

#' list that automatically names its elements
#' @references http://stackoverflow.com/a/21059868
#' @export
named_list <- function(...){
  x <- list(...)
  inferred <- sapply(substitute(list(...)), function(y) deparse(y)[1])[-1]
  names(x) <- inferred
  x
}
