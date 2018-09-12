test_that('question functions', {
  h <- 1
  # boards_arr[,,h]
  expect_equal(q.shipsize(h, 1), 2)
  expect_equal(q.horizontal(h, 1), 0)
  expect_equal(q.location(h, 1, 1), 1)
  expect_equal(q.touching(h, 1, 2), 1)
  expect_equal(q.tiles(h, 1), '1-1 2-1')
  expect_equal(q.tiles(h, 0), '3-1 4-1 5-1 6-1 3-2 4-2 5-2 6-2 3-3 4-3 5-3 6-3 1-4 2-4 3-4 4-4 5-4 6-4 1-5 2-5 3-5 4-5 5-5 6-5 1-6 2-6 3-6 4-6 5-6 6-6')

  h <- c(100, 20000)
  # boards_arr[,,h]
  expect_equal(q.shipsize(h, 1), c(2, 2))
  expect_equal(q.horizontal(h, 1), c(0, 0))
  expect_equal(q.location(h, 1, 1), c(1, 0))
  expect_equal(q.touching(h, 1, 2), c(1, 1))
  expect_equal(q.tiles(h, 1), c('1-1 2-1', '1-2 2-2'))
})
