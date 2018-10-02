test_that('grammar', {
  expect_equal(create_rewrite_rule('A -> B'), data_frame(left = 'A', right = 'B'))
  expect_equal(create_rewrite_rules('
   A -> B
   A -> C'), data_frame(left = c('A', 'A'), right = c('B', 'C')))

  cg <- create_grammar()
  expect_named(cg, c("nonterminals", "terminals", "rewrite_rules", "start_symbol"))
})
