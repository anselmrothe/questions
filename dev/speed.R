e <- function(){
  sprintf('Non-integer in %s', x)
}
f <- function(){
  glue('Non-integer in {x}')
}
g <- function(){
}

comparison <- microbenchmark::microbenchmark(e(),
                                             f(),
                                             # g(),
                                             times = 100)
ggplot2::autoplot(comparison)
comparison
