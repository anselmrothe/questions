
test_that('can run pblapply', {
  expect_equal(c(1:10) %>% pbapply::pblapply(mean),
               c(1:10) %>% lapply(mean))
})

test_that('can run pbmclapply', {
  expect_equal(c(1:10) %>% pbmcapply::pbmclapply(mean),
               c(1:10) %>% lapply(mean))
})

test_that('battleship variables', {
  ## these global variables should exist after running 'library(questions)'
  expect_true(exists('ROWS'))
  expect_true(exists('COLS'))

  expect_is(ROWS, 'integer')
  expect_is(COLS, 'integer')
  expect_is(df.coords, 'data.frame')
  expect_is(COORDS, 'character')
  expect_is(GRID, 'matrix')
  expect_is(neighbor_tiles, 'list')
  expect_is(touching_tiles, 'data.frame')
  expect_is(SHIPS, 'integer')
  expect_is(SIZES, 'integer')
  expect_is(ORIENTATIONS, 'character')

  ## alternative way to access variables
  expect_is(questions::ROWS, 'integer')

  ## consistency between COORDS and GRID
  expect_equal(COORDS[GRID[3,1]], '3-1')
  expect_equal(COORDS[GRID[4,5]], '4-5')

  ## create battleship variables
  v <- create_battleship_variables()
  expect_equal(v$ROWS, ROWS)
  expect_equal(v$COLS, COLS)
  expect_equal(v$df.coords, df.coords)
  expect_equal(v$COORDS, COORDS)
  expect_equal(v$GRID, GRID)
  expect_equal(v$neighbor_tiles, neighbor_tiles)
  expect_equal(v$touching_tiles, touching_tiles)
  expect_equal(v$SHIPS, SHIPS)
  expect_equal(v$SIZES, SIZES)
  expect_equal(v$ORIENTATIONS, ORIENTATIONS)

  expect_equal(create_neighbor_tiles(GRID) %>% length, 36)
  expect_equal(create_touching_tiles(ROWS, COLS, GRID) %>% dim, c(60, 2))
})

test_that('battleship game boards', {
  bos <- boards_one_ship()
  expect_is(bos, 'data.frame')
  expect_is(bos$board, 'list')
  expect_is(bos$board[[1]], 'matrix')
})

test_that('coord', {
  expect_equal(coord(1, 2), '1-2')
  expect_equal(coord_row('1-2'), 1)
  expect_equal(coord_col('1-2'), 2)
})
