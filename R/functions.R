
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
  dots <- list(...)
  inferred <- sapply(substitute(list(...)), function(x) deparse(x)[1])[-1]
  if(is.null(names(inferred))){
    names(dots) <- inferred
  } else {
    names(dots)[names(inferred) == ""] <- inferred[names(inferred) == ""]
  }
  dots
}
