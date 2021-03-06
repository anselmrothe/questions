## these global variables should exist after running 'library(questions)'

test_that('battleship.R', {
  expect_true(exists('NROWS'))

  expect_is(NROWS, 'numeric')
  expect_is(NCOLS, 'numeric')
  expect_is(df.coords, 'data.frame')
  expect_is(COORDS, 'character')
  expect_is(ROWS, 'matrix')
  expect_is(COLS, 'matrix')
  expect_is(GRID, 'matrix')
  expect_is(neighbor_tiles, 'list')
  expect_is(touching_tiles, 'data.frame')
  expect_is(SHIPS, 'integer')
  expect_is(SIZES, 'integer')
  expect_is(boards, 'data.frame')
  expect_is(boards_arr, 'array')
  expect_is(N, 'integer')

  ## alternative way to access variables
  expect_is(questions::NROWS, 'numeric')

  ## consistency between COORDS and GRID
  expect_equal(COORDS[GRID[3,1]], '3A')
  expect_equal(COORDS[GRID[4,6]], '4F')

  ## neighbor_tiles has names
  expect_named(neighbor_tiles, COORDS)

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

test_that('answers', {
  ## answers.R
  expect_is(answers, 'data.frame')
  expect_equal(nrow(answers), N)
})
