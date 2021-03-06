
test_that('prior_uniform_ship_sizes', {
  pus <- prior_uniform_ship_sizes(testing = TRUE)
  expect_equal(sum(pus$uniform_prior), 1)
  expect_equal(sum(pus$uniform_shipsizes_prior), 1)

  expect_true(all(colnames(pus) %in% colnames(boards2)))
  expect_equal(sum(boards2$uniform_prior), 1)
  expect_equal(sum(boards2$uniform_shipsizes_prior), 1)
})

test_that('boards_subset', {
  bs <- boards_subset(101:120)
  expect_is(bs, 'list')
  expect_equal(bs$boards2$uniform_prior %>% sum, 1)
  expect_equal(bs$boards2$uniform_shipsizes_prior %>% sum, 1)
})
