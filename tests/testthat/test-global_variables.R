## these global variables should exist after running 'library(questions)'

test_that('battleship.R', {
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
  expect_is(boards, 'data.frame')
  expect_is(boards_arr, 'array')
  expect_is(N, 'integer')

  ## alternative way to access variables
  expect_is(questions::ROWS, 'integer')

  ## consistency between COORDS and GRID
  expect_equal(COORDS[GRID[3,1]], '3-1')
  expect_equal(COORDS[GRID[4,5]], '4-5')

  ## double check game boards
  expect_equal(N, 1653456)
  expect_equal(dim(boards_arr), c(6, 6, N))
  expect_equal(nrow(boards), 3 * N)
})

test_that('boards2', {
  ## agent_bayes.R
  expect_is(boards2, 'data.frame')
  expect_equal(nrow(boards2), N)
})
