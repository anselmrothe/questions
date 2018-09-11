test_that('question functions', {
  h <- 1
  # boards_arr[,,h]

  expect_equal(q.horizontal(h, 1), 0)
  expect_equal(q.horizontal(h, 2), 0)
})
