# test-download-data.R

context("Authentication")

test_that("Facebook access token is accessible", {
  tk <- find_token()

  expect_is(tk, "fbTokenObj")
  expect_s3_class(tk, "fbTokenObj")
})

test_that("Facebook access token expiry can be checked", {
  expiry <- token_expiry()

  expect_type(expiry, "double")
  expect_is(expiry, "Date")
})



context("Data display")

test_that('input file is validatated', {
  expect_error(show_datasets(), 'is missing, with no default')
  expect_error(show_datasets('nofile.db'), 'does not exist')
})
