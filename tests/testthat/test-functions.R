
test_that('normalize', {
  expect_equal(normalize(1), 1)
  expect_equal(normalize(c(1, 1)), c(.5, .5))
  expect_equal(normalize(c(0, 1)), c(0, 1))
  expect_equal(normalize(c(1, 3)), c(.25, .75))
  expect_equal(normalize(0), 0)
})

test_that('vec_chr', {
  expect_equal(vec_chr(1), '1')
  expect_equal(vec_chr(c(1, 2)), '1 2')
  expect_equal(vec_chr(c('a', 'b')), 'a b')
  expect_equal(vec_chr(c('a', 2)), 'a 2')
})

test_that('chr_vec', {
  expect_equal(chr_vec('1'), '1')
  expect_equal(chr_vec('1 2'), c('1', '2'))
  expect_equal(chr_vec('a b'), c('a', 'b'))
  expect_equal(chr_vec('a 2'), c('a', '2'))
})

test_that('chr_vec_int', {
  expect_equal(chr_vec_int('1'), 1)
  expect_equal(chr_vec_int('1 2'), 1:2)
  expect_error(chr_vec_int('a b'))
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

test_that('text_to_data_frame', {
  text <- '
  a     b
  first second'
  expect_equal(text_to_data_frame(text),
               data_frame(a = 'first', b = 'second'))
})
