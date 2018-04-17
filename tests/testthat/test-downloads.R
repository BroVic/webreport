# test-download-data.R

context("Authentication")

test_that("Facebook access token is accessible", {
  tk <- find_token()

  expect_is(tk, "fbTokenObj")
  expect_s3_class(tk, "fbTokenObj")
})

