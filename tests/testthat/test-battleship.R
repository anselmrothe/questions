
test_that('battleship variables exist', {
  ## these global variables should exist after running 'library(questions)'
  expect_true(exists('ROWS'))
  expect_true(exists('COLS'))
  expect_true(exists('df.coords'))
  expect_true(exists('COORDS'))
  expect_true(exists('GRID'))
  expect_true(exists('neighbor_tiles'))
  expect_true(exists('touching_tiles'))
  expect_true(exists('ship_shapes'))

  expect_is(ROWS, 'integer')
  expect_is(COLS, 'integer')
  expect_is(df.coords, 'data.frame')
  expect_is(COORDS, 'character')
  expect_is(GRID, 'matrix')
  expect_is(neighbor_tiles, 'list')
  expect_is(touching_tiles, 'data.frame')
  expect_is(ship_shapes, 'data.frame')

  ## alternative way to access variables
  expect_is(questions::ROWS, 'integer')

  ## create battleship variables
  v <- create_battleship_variables()
  expect_equal(v$ROWS, ROWS)
  expect_equal(v$df.coords, df.coords)
  expect_equal(v$neighbor_tiles, neighbor_tiles)

  expect_equal(create_neighbor_tiles(GRID) %>% length, 36)
  expect_equal(create_touching_tiles(ROWS, COLS, GRID) %>% dim, c(60, 2))
})
