## precompute answers for efficiency

compute_answers_lookup_table <- function() {
  # answers <- data_frame(id = 1:N)

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

  # devtools::use_data(answers, overwrite = TRUE, compress = 'gzip')
}
