test_that('normalizing', {
  expect_equal(normalizing(1), 1)
  expect_equal(normalizing(c(1, 1)), c(.5, .5))
  expect_equal(normalizing(c(0, 1)), c(0, 1))
  expect_equal(normalizing(c(1, 3)), c(.25, .75))
  expect_equal(normalizing(0), 0)
})