
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
