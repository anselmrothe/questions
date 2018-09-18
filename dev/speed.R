e <- function(){
  q.shiptiles(1:50, 1)
}
f <- function(){
  q.shiptiles2(1:50, 1)
}
g <- function(){
}

comparison <- microbenchmark::microbenchmark(e(),
                                             f(),
                                             # g(),
                                             times = 100)
ggplot2::autoplot(comparison)
comparison
