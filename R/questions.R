## these question functions are vectorized over h

q.shipsize <- function(h, ship) {
  key <- vec_chr(c('q.shipsize', ship))
  getElement(answers, key)[h]
}
q.horizontal <- function(h, ship) {
  key <- vec_chr(c('q.horizontal', ship))
  getElement(answers, key)[h]
}
