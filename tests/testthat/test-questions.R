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

test_that('question variables', {
  expect_equal(question_var_num('1'), 1)
  expect_error(question_var_num('A'), 'Could not recode A to a number')
  expect_equal(question_var_num('2B'), 8)
  expect_equal(question_var_num('Water'), 0)

  expect_equal(question_var_chr(1, 'boolean'), 'True')
  expect_equal(question_var_chr(3, 'location'), '3A')
  expect_equal(question_var_chr(3, 'color'), 'Purple')
  expect_equal(question_var_chr(1, 'orientation'), 'Horizontal')
  expect_error(question_var_chr(3, 'orientation'), "Could not recode '3' to type 'orientation'")
  expect_error(question_var_chr(99, 'location'), "Could not recode '99' to type 'location'")
})
