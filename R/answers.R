## precompute answers for efficiency

compute_answers_lookup_table <- function(testing = FALSE) {
  # answers <- data_frame(id = 1:N)

  if (testing) {
    test_ids <- 1:10
    answers <- data_frame(id = test_ids)
    boards <- boards %>% filter(id %in% test_ids)
    boards_arr <- boards_arr[,,test_ids]
  }

  type <- 'q.shipsize'
  for (shipx in 1:3) {
    key <-  vec_chr(c(type, shipx))
    answ <- filter(boards, ship == shipx)$size
    stopifnot(length(answ) == nrow(answers))
    answers[,key] <- answ
  }

  type <- 'q.horizontal'
  for (shipx in 1:3) {
    key <-  vec_chr(c(type, shipx))
    answ <- (filter(boards, ship == shipx)$orientation == 'horizontal') * 1
    stopifnot(length(answ) == nrow(answers))
    answers[,key] <- answ
  }

  ## 410 sec
  system.time({
    type <- 'q.touching'
    dd <- data_frame(ship1 = c(1, 1, 2), ship2 = c(2, 3, 3))
    for (x in list_of_rows(dd)) {
      key <- vec_chr(c(type, x$ship1, x$ship2))
      ship1_neighbors <- boards %>% filter(ship == x$ship1) %>% .$neighbors
      ship2_coords <- boards %>% filter(ship == x$ship2) %>% .$coords
      ## 150 sec
      # system.time({
      answ <- map2_dbl(ship1_neighbors, ship2_coords, a.touching)
      # })
      stopifnot(length(answ) == nrow(answers))
      answers[,key] <- answ
    }
  })

  type <- 'q.tiles'
  color <- 0
  key <-  vec_chr(c(type, color))
  ## 30 sec
  system.time({
    f <- function(board) vec_chr(COORDS[board == color])
    answ <- boards_arr %>% apply(3, f)
  })
  stopifnot(length(answ) == nrow(answers))
  answers[,key] <- answ
  for (color in 1:3) {
    key <-  vec_chr(c(type, color))
    answ <- filter(boards, ship == color)$coords
    stopifnot(length(answ) == nrow(answers))
    answers[,key] <- answ
  }

  # devtools::use_data(answers, overwrite = TRUE, compress = 'gzip')

  answers
}


## these question functions are NOT vectorized over h

a.touching <- function(ship1_neighbors_chr, ship2_coords_chr) {
  (any(chr_vec(ship1_neighbors_chr) %in% chr_vec(ship2_coords_chr)) * 1)
}
