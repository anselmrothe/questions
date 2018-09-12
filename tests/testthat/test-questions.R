test_that('question functions', {
  h <- 1
  # boards_arr[,,h]

  expect_equal(q.shipsize(h, 1), 2)
  expect_equal(q.horizontal(h, 1), 0)
  expect_equal(q.location(h, 1, 1), 1)
  expect_equal(q.touching(h, 1, 2), 1)

  h <- c(100, 20000)
  # boards_arr[,,h]
  expect_equal(q.shipsize(h, 1), c(2, 2))
  expect_equal(q.horizontal(h, 1), c(0, 0))
  expect_equal(q.location(h, 1, 1), c(1, 0))
  expect_equal(q.touching(h, 1, 2), c(1, 1))
})
