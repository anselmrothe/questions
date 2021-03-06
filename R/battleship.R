### functions concerning the battleship game environment

#' create the hypothesis space of 1.6 million game boards
create_gameboards <- function(testing = FALSE) {
  bos1 <- boards_one_ship(ship_label = 1)
  bos2 <- boards_one_ship(ship_label = 2)
  bos3 <- boards_one_ship(ship_label = 3)

  crs <- crossing(bos1, bos2, bos3)  ## 2.9M rows

  if (testing) {
    crs <- crs[1080:1100,]
  }

  ## convert to arrays for speedup
  ## 70 sec
  system.time({
    f <- function(x) x %>% unlist %>% array(dim = c(6, 6, nrow(crs)))
    arr1 <- crs$board %>% f
    arr2 <- crs$board1 %>% f
    arr3 <- crs$board2 %>% f
  })
  # arr1[,,1]

  ## multiply arrays with each other to find overlaps
  ## 30 sec
  system.time({
    f <- function(x) (x * 10 + 1)
    arr <- (f(arr1) * f(arr2) * f(arr3) - 1) / 10
    arr_overlap <- arr > 3  ## overlapping ships result in value > 3
    overlap <- arr_overlap %>% apply(3, any)
  })

  boards_arr <-arr[,,!overlap]
  valid_boards <- crs[!overlap,]


  ## reshape infos for each ship
  dd <- valid_boards %>% mutate(id = 1:nrow(valid_boards))
  d1 <- dd %>% select(id, ship:neighbors)
  d2 <- dd %>% select(id, ship1:neighbors1)
  d3 <- dd %>% select(id, ship2:neighbors2)
  colnames(d2) <- colnames(d1)
  colnames(d3) <- colnames(d1)
  boards <- bind_rows(d1, d2, d3) %>% arrange(id)

  N <- dim(boards_arr)[3]

  ## run this only once
  # devtools::use_data(boards_arr, overwrite = TRUE, compress = 'gzip')
  # devtools::use_data(boards, overwrite = TRUE, compress = 'gzip')
  # devtools::use_data(N, overwrite = TRUE)

  named_list(boards, boards_arr, N)
}

boards_one_ship <- function(ship_label = 1) {
  ## combinations of ship shapes and positions
  ## filter out positions where ship extends beyond board size
  board_setting <- crossing(ship = ship_label, size = SIZES, horizontal = 0:1, topleft_row = 1:6, topleft_col = 1:6) %>%
    mutate(bottomright_col = ifelse(horizontal == 1, topleft_col + size - 1, topleft_col),
           bottomright_row = ifelse(horizontal == 1, topleft_row, topleft_row + size - 1),
           valid = ifelse(bottomright_col > 6, FALSE, TRUE),
           valid = ifelse(bottomright_row > 6, FALSE, valid)) %>%
    filter(valid) %>%
    select(-valid) %>%
    arrange(horizontal, size, topleft_row, topleft_col)  ## ordering for compatibility with old code

  ## add coordinates
  board_setting <- board_setting %>%
    rowwise %>%
    mutate(coords = COORDS[GRID[topleft_row:bottomright_row,
                                topleft_col:bottomright_col]] %>% vec_chr,
           topleft = coord(topleft_row, topleft_col),
           bottomright = coord(bottomright_row, bottomright_col),
           neighbors = compute_neighbors_chr(coords)) %>%
    ungroup

  ## paint ship labels into boards
  ## board = game board = grid = conf = configuration = 6x6 matrix
  setting_to_board <- function(x) {
    b <- matrix(numeric(36), ncol = 6) ## empty board
    b[x$topleft_row:x$bottomright_row,
      x$topleft_col:x$bottomright_col] <- ship_label
    b
  }
  boards <- board_setting %>% list_of_rows %>% lapply(setting_to_board)

  ## combine board_settings with list of boards
  board_setting %>% mutate(board = boards)
}

create_battleship_variables <- function() {
  NROWS <- 6
  NCOLS <- 6
  df.coords <- tidyr::crossing(row = 1:NROWS, col = 1:NCOLS) %>%
    arrange(col, row) %>%
    mutate(coords = coord(row, col))
  COORDS <- df.coords$coords
  ROWS <- matrix(df.coords$row, ncol = NCOLS, byrow = FALSE)  ## we go by column
  COLS <- matrix(df.coords$col, ncol = NCOLS, byrow = FALSE)  ## we go by column
  GRID <- matrix(1:36, ncol = NCOLS, byrow = FALSE)  ## we go by column
  neighbor_tiles <- create_neighbor_tiles(GRID, COORDS)
  touching_tiles <- create_touching_tiles(GRID)

  SHIPS <- 1:3
  SIZES <- 2:4

  # # run this only once
  # devtools::use_data(NROWS, overwrite = TRUE)
  # devtools::use_data(NCOLS, overwrite = TRUE)
  # devtools::use_data(df.coords, overwrite = TRUE)
  # devtools::use_data(COORDS, overwrite = TRUE)
  # devtools::use_data(ROWS, overwrite = TRUE)
  # devtools::use_data(COLS, overwrite = TRUE)
  # devtools::use_data(GRID, overwrite = TRUE)
  # devtools::use_data(neighbor_tiles, overwrite = TRUE)
  # devtools::use_data(touching_tiles, overwrite = TRUE)
  # devtools::use_data(SHIPS, overwrite = TRUE)
  # devtools::use_data(SIZES, overwrite = TRUE)

  named_list(NROWS,
             NCOLS,
             df.coords,
             COORDS,
             ROWS,
             COLS,
             GRID,
             neighbor_tiles,
             touching_tiles,
             SHIPS,
             SIZES)
}

create_neighbor_tiles <- function(GRID, COORDS) {
  neighbors <- data_frame(tile = 1:36) %>%
    mutate(top=   tile-1,
           bottom=tile+1,
           left=  tile-6,
           right= tile+6)
  ## these neighbors are not possible
  neighbors[GRID[1,],"top"] <- NA
  neighbors[GRID[6,],"bottom"] <- NA
  neighbors[GRID[,1],"left"] <- NA
  neighbors[GRID[,6],"right"] <- NA

  ## make list of only possible neighbors
  f <- function(tile) {
    x <- neighbors[tile,] %>% select(top, right, bottom, left) %>% unlist %>% unname
    x[!is.na(x)]
  }
  y <- neighbors$tile %>% lapply(f)
  names(y) <- COORDS
  y
}

create_touching_tiles <- function(GRID) {
  df <- data.frame(i = 0, j = 0)[-1,]
  for (col in 1:6) {
    for (row in 1:5) {
      i <- GRID[row,col]
      j <- GRID[row+1,col]
      df <- rbind(df, data_frame(i, j))
    }
  }
  for (row in 1:6) {
    for (col in 1:5) {
      i <- GRID[row,col]
      j <- GRID[row,col+1]
      df <- rbind(df, data_frame(i, j))
    }
  }
  df
}

compute_neighbors_chr <- function(coords_chr) {
  f <- function(key, object) getElement(object, key)
  x <- coords_chr %>% chr_vec %>% lapply(f, neighbor_tiles) %>% unlist %>% unique %>% sort
  COORDS[x] %>% vec_chr
}

# format ------------------------------------------------------------------

#' coordinate format
#' @export
coord <- function(row, col) paste0(row, LETTERS[col])

#' coordinate format
#' @export
coord_row <- function(coord) as.integer(substr(coord, 1, 1))

#' coordinate format
#' @export
coord_col <- function(coord) as.integer(which(LETTERS == substr(coord, 2, 2)))
