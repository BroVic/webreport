# test-plot_ts.R

context('Time-series plotting')

## Objects used for these tests
dat <- readRDS('test-data/ntweets.rds')
result <- platformSpecs(dat, 'twitter')
df <- result$data
begDate <- min(df[[result$date.colName]])
endDate <- max(df[[result$date.colName]])
yr <- suppressWarnings(.numericalDateElem(begDate, '%Y'))
mth <- suppressWarnings(.numericalDateElem(begDate, '%m'))
dmatrix <- prepare(result, 'ecomsaoauife')
tmsr <- zoo(dmatrix, order.by = seq(begDate, endDate, by = 'day'))

## Tests proper
test_that('input is validated', {
  expect_error(make_ts(), '"data" is missing, with no default')
  expect_error(make_ts(data = matrix(LETTERS, 2)),
               'inherits(data, "data.frame") is not TRUE',
               fixed = TRUE)
  expect_error(make_ts(mtcars),
               "argument \"platform\" is missing, with no default")
  expect_error(make_ts())
  expect_error(
    object = make_ts(dat, 'twitter', startDate = as.character(Sys.Date())),
    info = 'inherits(startDate, "Date") is not TRUE',
    fixed = TRUE
  )
  expect_error(
    object = make_ts(dat, 'twitter', startDate = as.numeric(Sys.Date())),
    info = 'inherits(startDate, "Date") is not TRUE',
    fixed = TRUE
  )
  expect_error(
    make_ts(dat, 'twitter', startDate = NA),
    info = 'inherits(startDate, "Date") is not TRUE',
    fixed = TRUE
  )
  expect_error(
    make_ts(dat, 'twitter', startDate = NULL),
    'inherits(startDate, "Date") is not TRUE',
    fixed = TRUE
  )
  expect_error(
    object = make_ts(dat, 'twitter', startDate = as.POSIXct(Sys.Date())),
    info = 'inherits(startDate, "Date") is not TRUE',
    fixed = TRUE
  )
})

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
  expect_error(.numericalDateElem(),
               'argument "placeholder" is missing')
  expect_error(.numericalDateElem(placeholder = 0),
               'is.character(placeholder) is not TRUE',
               fixed = TRUE)
  expect_error(.numericalDateElem(placeholder = 'error'),
               'grepl("^%[A-Za-z]{1}", placeholder) is not TRUE',
               fixed = TRUE)
  expect_warning(.numericalDateElem(Sys.Date() - 400, '%Y'),
                 'You have elected to use a time-frame larger than 1 year')
  expect_warning(.numericalDateElem(Sys.Date(), '%yy'),
                 'NAs introduced by coercion')
  expect_error(.numericalDateElem(date = '2018-01-01', placeholder = '%b'),
               'inherits(date, "Date") is not TRUE',
               fixed = TRUE)
})

test_that('Numerical date values are in order', {
  expect_type(yr, 'double')
  expect_type(mth, 'double')
  expect_is(yr, 'numeric')
  expect_is(mth, 'numeric')
  expect_equal(yr, as.numeric(format(begDate, '%Y')))
  expect_equal(mth, as.numeric(format(begDate, '%m')))
})

test_that('Time series object is successfully created', {
  expect_error(prepare(specs = result))
  expect_error(prepare(sender = 'ecomsaoauife'))
  expect_error(
    prepare(mtcars, 'NESREANigeria'),
    "unused argument (\"NESREANigeria\")",
    fixed = TRUE
  )
  ## Success test cases
  expect_type(dmatrix, 'integer')
  expect_is(dmatrix, 'matrix')
})

test_that('Input for time-series plotting is validated', {
  expect_error(.drawTimeSeries(1:10, specs = result),
               'inherits(zObj, "zoo") is not TRUE',
               fixed = TRUE)
  expect_error(.drawTimeSeries(tmsr, specs = list()),
               'inherits(specs, "platformSpecs") is not TRUE',
               fixed = TRUE)
})
