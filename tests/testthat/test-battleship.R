
test_that('can run pblapply', {
  expect_equal(c(1:10) %>% pbapply::pblapply(mean),
               c(1:10) %>% lapply(mean))
})

test_that('can run pbmclapply', {
  expect_equal(c(1:10) %>% pbmcapply::pbmclapply(mean),
               c(1:10) %>% lapply(mean))
})

test_that('create battleship variables', {
  v <- create_battleship_variables()
  expect_equal(v$NROWS, NROWS)
  expect_equal(v$NCOLS, NCOLS)
  expect_equal(v$df.coords, df.coords)
  expect_equal(v$COORDS, COORDS)
  expect_equal(v$ROWS, ROWS)
  expect_equal(v$COLS, COLS)
  expect_equal(v$GRID, GRID)
  expect_equal(v$neighbor_tiles, neighbor_tiles)
  expect_equal(v$touching_tiles, touching_tiles)
  expect_equal(v$SHIPS, SHIPS)
  expect_equal(v$SIZES, SIZES)

  expect_equal(create_neighbor_tiles(GRID, COORDS) %>% length, 36)
  expect_equal(create_touching_tiles(GRID) %>% dim, c(60, 2))
})

test_that('battleship game boards', {
  bos <- boards_one_ship()
  expect_is(bos, 'data.frame')
  expect_is(bos$board, 'list')
  expect_is(bos$board[[1]], 'matrix')
  expect_is(bos$neighbors, 'character')

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

test_that('compute_neighbors_chr', {
  expect_equal(compute_neighbors_chr('1A'), '2A 1B')
  expect_equal(compute_neighbors_chr('1A 6F'), '2A 1B 6E 5F')
})

test_that('coord', {
  expect_equal(coord(1, 2), '1B')
  expect_equal(coord(1:2, 5:6), c('1E', '2F'))
  expect_equal(coord_row('1B'), 1)
  expect_equal(coord_col('1B'), 2)
})
