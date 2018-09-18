
create_grammar <- function() {
  ## G = (V, E, R, S)
  ## G: the grammar
  ## V: nonterminal symbols (will be rewritten)
  ## E: terminal symbols (will not be rewritten)
  ## R: rewrite rules
  ## S: start symbol (is element of V)

  start_symbol <- 'A'
  nonterminals <- chr_vec('A B')
  terminals <- chr_vec('True False not')
  rewrite_rules <- add_rewrite_rules('
A -> B
B -> True
B -> False
B -> (not B)
')
  rewrite_rules <- text_to_data_frame

  ## check consistency
  stopifnot(start_symbol %in% nonterminals)
  stopifnot(!any(terminals %in% nonterminals))
}

add_rewrite_rules <- function(text) {
  ## split up lines
  x <- stringr::str_split(text, pattern = '\n')[[1]]
  ## remove empty lines
  x <- x[x != '']
  add_rewrite_rule <- function(line) {
    line
  }
  ## split by ' -> '
  stringr::str_split(text, pattern = ' -> ')[[1]]
}

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
