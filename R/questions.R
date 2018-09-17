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


## question formatting

## convert variable from character string to number
question_var_num <- function(x) {
  if(x == "True")              y <- 1
  else if(x == "False")        y <- 0
  else if(x == "Horizontal")   y <- 1
  else if(x == "Vertical")     y <- 0
  else if(x == "Water")        y <- 0
  else if(x == "Blue")         y <- 1
  else if(x == "Red")          y <- 2
  else if(x == "Purple")       y <- 3
  else if(any(COORDS == x))    y <- which(COORDS == x)
  else                         y <- suppressWarnings(as.numeric(x))
  if (is.na(y)) stop(sprintf('Could not recode %s to a number', x))
  y
}

## convert variable from number to character string
question_var_chr <- function(x, type) {
  y <- NA
  if (type == 'boolean') {
    if (x == 1) y <- 'True'
    if (x == 0) y <- 'False'

  } else if (type == 'number') {
                y <- as.character(x)

  } else if (type == 'color') {
    if (x == 0) y <- 'Water'
    if (x == 1) y <- 'Blue'
    if (x == 2) y <- 'Red'
    if (x == 3) y <- 'Purple'

  } else if (type == 'orientation') {
    if (x == 1) y <- 'Horizontal'
    if (x == 0) y <- 'Vertical'

  } else if (type == 'location') {
                y <- COORDS[x]
  }
  if (is.na(y)) stop(sprintf("Could not recode '%s' to type '%s'", x, type))
  y
}
