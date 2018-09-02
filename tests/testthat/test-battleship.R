
test_that('battleship variables', {
  ## 'v' is a list of precomputed variables
  ## 'v' exists after running library(questions)
  expect_true(exists('v'))
  expect_is(v, 'list')
  expect_is(questions::v, 'list')
  expect_named(v, c('ROWS', 'COLS', 'df.coords', 'COORDS', 'GRID',
                    'neighbor_tiles', 'ship_shapes'))
  expect_equal(v, create_battleship_variables())

  expect_equal(create_neighbor_tiles(v$GRID) %>% length, 36)
  expect_equal(create_touching_tiles(v$ROWS, v$COLS, v$GRID) %>% dim, c(60, 2))
})
