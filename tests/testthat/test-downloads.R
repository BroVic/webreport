# test-download-data.R

context("Authentication")

test_that("Facebook access token is accessible", {
  tk <- findToken()

  expect_is(tk, "fbTokenObj")
  expect_s3_class(tk, "fbTokenObj")
})

context("Facebook token renewals")
expiry <- token_expiry()
test_that("Facebook access token expiry can be checked", {
  expect_type(expiry, "double")
  expect_is(expiry, "Date")
})

test_that("Facebook access token can be renewed", {
  expect_type(renew_fb_cred(), 'logical')
  if (expiry > Sys.Date()) {
    expect_message(renew_fb_cred(), 'has not yet expired')
  }
})

context("Data display")

test_that('input file is validatated', {
  expect_error(show_datasets(), 'is missing, with no default')
  expect_error(show_datasets('nofile.db'), 'does not exist')
})
