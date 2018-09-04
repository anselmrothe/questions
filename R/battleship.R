
#' create the hypothesis space of 1.6 million game boards
#' @export
create_gameboards <- function(x) {
  ## TODO
}

boards_one_ship <- function(ship_label = 1) {
  ## combinations of ship shapes and positions
  ## filter out positions where ship extends beyond board size
  board_setting <- crossing(ship = ship_label, size = SIZES, orientation = ORIENTATIONS, topleft_row = ROWS, topleft_col = COLS) %>%
    mutate(bottomright_col = ifelse(orientation == 'horizontal', topleft_col + size - 1, topleft_col),
           bottomright_row = ifelse(orientation == 'horizontal', topleft_row, topleft_row + size - 1),
           valid = ifelse(bottomright_col > 6, FALSE, TRUE),
           valid = ifelse(bottomright_row > 6, FALSE, valid)) %>%
    filter(valid) %>%
    arrange(size, desc(orientation), topleft_row, topleft_col)  ## ordering for compatibility with old code

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
  ROWS <- 1:6
  COLS <- 1:6
  df.coords <- tidyr::crossing(row = ROWS, col = COLS) %>%
    arrange(col, row) %>%
    mutate(coords = sprintf('%s-%s', row, col))
  COORDS <- df.coords$coords
  GRID <- matrix(1:36, ncol = 6, byrow = FALSE)  ## we go by column
  neighbor_tiles <- create_neighbor_tiles(GRID)
  touching_tiles <- create_touching_tiles(ROWS, COLS, GRID)

  SHIPS <- 1:3
  SIZES <- 2:4
  ORIENTATIONS <- c('horizontal', 'vertical')

  ## run this only once
  # devtools::use_data(ROWS, overwrite = TRUE)
  # devtools::use_data(COLS, overwrite = TRUE)
  # devtools::use_data(df.coords, overwrite = TRUE)
  # devtools::use_data(COORDS, overwrite = TRUE)
  # devtools::use_data(GRID, overwrite = TRUE)
  # devtools::use_data(neighbor_tiles, overwrite = TRUE)
  # devtools::use_data(touching_tiles, overwrite = TRUE)
  # devtools::use_data(SHIPS, overwrite = TRUE)
  # devtools::use_data(SIZES, overwrite = TRUE)
  # devtools::use_data(ORIENTATIONS, overwrite = TRUE)

  named_list(ROWS,
             COLS,
             df.coords,
             COORDS,
             GRID,
             neighbor_tiles,
             touching_tiles,
             SHIPS,
             SIZES,
             ORIENTATIONS)
}

create_neighbor_tiles <- function(GRID) {
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
  neighbors$tile %>% lapply(f)
}

create_touching_tiles <- function(COLS, ROWS, GRID) {
  df <- data.frame(i = 0, j = 0)[-1,]
  for (col in COLS) {
    for (row in ROWS[-length(ROWS)]) {
      i <- GRID[row,col]
      j <- GRID[row+1,col]
      df <- rbind(df, data_frame(i, j))
    }
  }
  for (row in ROWS) {
    for (col in COLS[-length(COLS)]) {
      i <- GRID[row,col]
      j <- GRID[row,col+1]
      df <- rbind(df, data_frame(i, j))
    }
  }
  df
}
