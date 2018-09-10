
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

test_that('create battleship variables', {
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

  gb <- create_gameboards(testing = TRUE)
  expect_named(gb, c('boards', 'boards_arr', 'N'))
  expect_is(gb$boards, 'data.frame')
  expect_is(gb$boards_arr, 'array')
  # j <- 1
  # for (i in seq(dim(boards_arr)[3])) {
  #   if (isTRUE(all.equal(boards_arr[,,i], gb$boards_arr[,,j]))) {
  #     print(coord(i, j))
  #     j <- j + 1
  #   }
  # }
  # original_ids <- c(637,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652)
  expect_equal(gb$boards %>% filter(id == 1) %>% select(ship:coords),
               boards %>% filter(id == 637) %>% select(ship:coords))
  expect_equal(gb$boards_arr[,,1], boards_arr[,,637])
})

test_that('coord', {
  expect_equal(coord(1, 2), '1-2')
  expect_equal(coord(1:2, 6:7), c('1-6', '2-7'))
  expect_equal(coord_row('1-2'), 1)
  expect_equal(coord_col('1-2'), 2)
})
