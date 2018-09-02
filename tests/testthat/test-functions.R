
test_that('normalizing', {
  expect_equal(normalizing(1), 1)
  expect_equal(normalizing(c(1, 1)), c(.5, .5))
  expect_equal(normalizing(c(0, 1)), c(0, 1))
  expect_equal(normalizing(c(1, 3)), c(.25, .75))
  expect_equal(normalizing(0), 0)
})

test_that('named_list', {
  a <- 1
  expect_equal(named_list(a), list(a = a))
  expect_equal(named_list(a, a), list(a = a, a = a))
})

test_that('list_of_rows', {
  dd <- data_frame(x = c('a', 'b', 'c'))
  expect_equal(list_of_rows(dd)[[1]], data_frame(x = 'a'))
  expect_equal(list_of_rows(dd)[[2]], data_frame(x = 'b'))
  expect_equal(list_of_rows(dd)[[3]], data_frame(x = 'c'))
  expect_equal(list_of_rows(dd) %>% names, c('1', '2', '3'))
  expect_equal(list_of_rows(dd) %>% lapply(function(i) 'xx') %>% names, c('1', '2', '3'))
})
