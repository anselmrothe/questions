test_that('grammar', {
  expect_equal(func_arg_to_expr('const'), '(const)')
  expect_equal(func_arg_to_expr('size', 'Blue'), '(size Blue)')
  expect_equal(func_arg_to_expr('sum', '1', '2'), '(sum 1 2)')
})
