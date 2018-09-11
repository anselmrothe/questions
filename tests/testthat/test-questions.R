test_that('question functions', {
  h <- 1
  # boards_arr[,,h]

  expect_equal(q.shipsize(h, 1), 2)
  expect_equal(q.horizontal(h, 1), 0)

  h <- c(100, 20000)
  # boards_arr[,,h]
  expect_equal(q.shipsize(h, 1), c(2, 2))
  expect_equal(q.horizontal(h, 1), c(0, 0))

})
