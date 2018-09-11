q.horizontal <- function(h, shipx) {
  key <- vec_chr(c('q.horizontal', shipx))
  getElement(answers, key)[h]
}
