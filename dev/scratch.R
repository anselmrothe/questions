func_arg_to_expr <- function(functionname, arg1 = '', arg2 = '') {
  if (arg1 == '') {
    x <- functionname
  } else if (arg2 == '') {
    x <- paste(functionname, arg1)
  } else {
    x <- paste(functionname, arg1, arg2)
  }
  paste0('(', x, ')')
}
expect_equal(func_arg_to_expr('const'), '(const)')
expect_equal(func_arg_to_expr('size', 'Blue'), '(size Blue)')
expect_equal(func_arg_to_expr('sum', '1', '2'), '(sum 1 2)')


rewrite_rules <- function() {
  R <- NULL
  R <- R %>% add_rewrite_rule('A', 'B')
  R <- R %>% add_rewrite_rule('B', 'True')
  R
}

add_rewrite_rule <- function(R, left, symbol = '', functionname = '', arg1 = '', arg2 = '') {
  right <- symbol
  if (functionname != '') right <- func_arg_to_expr(functionname, arg1, arg2)
  r <- data_frame(left,
                  right,
                  functionname,
                  arg1,
                  arg2)
  rbind(R, r)
}

