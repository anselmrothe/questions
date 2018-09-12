## these question functions are vectorized over h

q.shipsize <- function(h, ship) {
  key <- vec_chr(c('q.shipsize', ship))
  getElement(answers, key)[h]
}
q.horizontal <- function(h, ship) {
  key <- vec_chr(c('q.horizontal', ship))
  getElement(answers, key)[h]
}
q.location <- function(h, row, col) {
  boards_arr[row,col,h]
}
q.touching <- function(h, ship1, ship2) {
  key <- vec_chr(c('q.touching', ship1, ship2))
  getElement(answers, key)[h]
}
q.shiptiles <- function(h, ship) {
  key <- vec_chr(c('q.shiptiles', ship))
  getElement(answers, key)[h]
}
