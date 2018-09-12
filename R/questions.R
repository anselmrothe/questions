## these question functions are all we need to let question programs to lookup
## information from the battleship boards

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
q.tiles <- function(h, color) {
  key <- vec_chr(c('q.tiles', color))
  getElement(answers, key)[h]
}
