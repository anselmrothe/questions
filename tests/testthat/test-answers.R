test_that('compute_answers_lookup_table', {
  calt <- compute_answers_lookup_table(testing = TRUE)
  expect_equal(names(calt), names(answers))
})

test_that('a.touching', {
  expect_equal(a.touching('1-1', '1-1'), 1)
  expect_equal(a.touching('1-1', '1-2'), 0)
  expect_equal(a.touching('1-1 1-2', '1-1 3-3'), 1)
})
