
create_grammar <- function() {
  start_symbol <- 'A'
  nonterminals <- chr_vec('A B')
  terminals <- chr_vec('True False not')
  rewrite_rules <- create_rewrite_rules('
A -> B
B -> True
B -> False
B -> (not B)
')
  ## uniform rule probabilities
  rewrite_rules <- rewrite_rules %>%
    mutate(prob = 1) %>%
    group_by(left) %>%
    mutate(prob = normalize(prob)) %>%
    ungroup

  ## check consistency
  stopifnot(start_symbol %in% nonterminals)
  stopifnot(!any(terminals %in% nonterminals))
  stopifnot(all(rewrite_rules$left %in% nonterminals))

  named_list(nonterminals, terminals, rewrite_rules, start_symbol)
}

create_rewrite_rules <- function(text) {
  x <- stringr::str_split(text, pattern = '\n')[[1]]
  x <- x[x != '']
  x %>% map_df(create_rewrite_rule)
}

create_rewrite_rule <- function(line) {
  y <- trimws(line)  ## trim leading/trailing whitespaces
  z <- stringr::str_split(y, pattern = ' -> ')[[1]]
  data_frame(left = z[1], right = z[2])
}


