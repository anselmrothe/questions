
test_that('prior_uniform_ship_sizes', {
  pus <- prior_uniform_ship_sizes(testing = TRUE)
  expect_equal(sum(pus$uniform_prior), 1)
  expect_equal(sum(pus$uniform_shipsizes_prior), 1)

  expect_true(all(colnames(pus) %in% colnames(boards2)))
  expect_equal(sum(boards2$uniform_prior), 1)
  expect_equal(sum(boards2$uniform_shipsizes_prior), 1)
})
