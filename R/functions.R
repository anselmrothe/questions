
#' normalize numbers such that they sum to 1
#' @export
normalize <- function(x) {
  if (sum(x)==0) {
    return(x)
  } else {
    return(x/sum(x))
  }
}

#' turn vector into single string
#' (elements are separated by space)
#' @export
vec_chr <- function(x) paste(x, collapse = ' ')

#' turn a string (of elements separated by space) into vector
#' @export
chr_vec <- function(x) stringr::str_split(x, ' ')[[1]]

#' turn a string (of elements separated by space) into vector of integers
#' @export
chr_vec_int <- function(x) {
  y <- suppressWarnings(purrr::map_int(chr_vec(x), as.integer))
  if (any(is.na(y))) stop(sprintf('Non-integer in %s', x))
  else y
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

#' Make a list of data frame rows
#' @export
list_of_rows <- function(df) {
  split(df, seq(nrow(df)))
}

#' Turn text into a data_frame
#' @export
text_to_data_frame <- function(text) {
  as_data_frame(read.table(header = TRUE, stringsAsFactors = FALSE, text = text))
}
