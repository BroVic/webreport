# test-plot_ts.R

context('Time-series plotting')

test_that('input is validated', {
  expect_error(make_ts(), '"data" is missing, with no default')
  expect_error(make_ts(data = matrix(LETTERS, 2)),
               'inherits(data, "data.frame") is not TRUE',
               fixed = TRUE)
  expect_error(make_ts(mtcars),
               "argument \"date.col\" is missing, with no default")
  expect_error(make_ts())
})
