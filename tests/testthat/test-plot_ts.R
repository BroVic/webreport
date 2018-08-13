# test-plot_ts.R

context('Time-series plotting')

test_that('input is validated', {
  expect_error(make_ts(), '"data" is missing, with no default')
  expect_error(make_ts(data = matrix(LETTERS, 2)),
               'inherits(data, "data.frame") is not TRUE',
               fixed = TRUE)
  expect_error(make_ts(mtcars),
               "argument \"platform\" is missing, with no default")
  expect_error(make_ts())
})

test_that('platform specific variables are set', {
  dat <- readRDS('test-data/ntweets.rds')

  expect_error(.preparePlatformSpecifics(),
               'argument "DATA" is missing, with no default')
  expect_error(.preparePlatformSpecifics(999),
               'argument "DATA" is missing, with no default')
  expect_error(.preparePlatformSpecifics(x = 999, DATA = dat),
               'is.character\\(x\\) is not TRUE')
  expect_error(.preparePlatformSpecifics(x = character(0), DATA = dat),
               "length(x) > 0 is not TRUE",
               fixed = TRUE)
  expect_error(
    .preparePlatformSpecifics('twitter', data.frame(a = letters, b = LETTERS)),
    "do not know how to convert '.' ",
    fixed = TRUE)
  expect_error(
    .preparePlatformSpecifics('twitter',
                              DATA = data.frame(a = letters, created = LETTERS)),
    'character string is not in a standard unambiguous format')
  expect_error(.preparePlatformSpecifics('unsupported', DATA = dat),
               "is not a supported platform",
               fixed = TRUE)
  ## Success checks
  result <- .preparePlatformSpecifics('twitter', dat)
  expect_type(result, 'list')
  expect_is(result, 'list')
  expect_is(result$data, 'data.frame')
  expect_type(result$date.column, 'character')
  expect_type(result$title.stub, 'character')
  expect_type(result$colour, 'character')
  expect_null(result$noexist)

  ## Inspect the data frame
  df <- result$data
  expect_equal(ncol(df), 16L)
  expect_true(nrow(df) > 0)
})
