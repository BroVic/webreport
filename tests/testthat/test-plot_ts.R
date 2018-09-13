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

dat <- readRDS('test-data/ntweets.rds')
result <- platformSpecs(dat, 'twitter')
df <- result$data
test_that('platform specific variables are set', {
  expect_error(platformSpecs(),
               'argument "DATA" is missing, with no default')
  expect_error(platformSpecs(999),
               'inherits(DATA, "data.frame") is not TRUE',
               fixed = TRUE)
  expect_error(platformSpecs(x = 999, DATA = dat),
               'is.character\\(x\\) is not TRUE')
  expect_error(platformSpecs(x = character(0), DATA = dat),
               "length(x) > 0 is not TRUE",
               fixed = TRUE)
  expect_error(platformSpecs(data.frame(a = letters, b = LETTERS), 'twitter'),
               "do not know how to convert 'DATA[[var]]' ",
               fixed = TRUE)
  expect_error(
    platformSpecs(DATA = data.frame(a = letters, created = LETTERS), 'twitter'),
    'character string is not in a standard unambiguous format'
  )
  expect_error(platformSpecs(DATA = dat, 'unsupported'),
               "is not a supported platform",
               fixed = TRUE)

  ## Success checks
  expect_type(result, 'list')
  expect_is(result, 'platformSpecs')
  expect_is(result$data, 'data.frame')
  expect_type(result$date.colName, 'character')
  expect_type(result$title.stub, 'character')
  expect_type(result$colour, 'character')
  expect_null(result$noexist)

  ## Inspect the data frame
  expect_equal(ncol(df), 16L)
  expect_true(nrow(df) > 0)
})

test_that('numerical values for date elements are correctly computed', {
  expect_error(.numericalDateElem())
  expect_error(.numericalDateElem(placeholder = 0))
  expect_error(.numericalDateElem(placeholder = 'error'))
  expect_warning(.numericalDateElem(placeholder = '%yy'))
  expect_error(.numericalDateElem(placeholder = '%b', dif = 'dif'))

  val <- .numericalDateElem('%Y')
  expect_type(val, 'double')
  expect_is(val, 'numeric')
  expect_equal(val, as.numeric(format(Sys.Date(), '%Y')))
})

test_that('matrix for time series is successfully created', {
  expect_error(prepare(specs = result))
  expect_error(prepare(sender = 'ecomsaoauife'))
  expect_error(
    prepare(mtcars, 'NESREANigeria'),
    "unused argument (\"NESREANigeria\")",
    fixed = TRUE
  )
  # expect_error(prepare(df, 'nonexistent'),
  #              "'sender' was not found in the dataset")

  ## Success test cases
  output <- prepare(result, 'ecomsaoauife')
  expect_type(output, 'integer')
  expect_is(output, 'matrix')
  expect_equal(dim(output)[2], 2L)
  expect_identical(colnames(output), c('allUpdates', 'bySender'))
})
