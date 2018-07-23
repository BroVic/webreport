# test-build_webreport.R

context("Report generation")

test_that("appropriate data are sourced", {
  expect_error(build_webreport())
  expect_error(build_webreport(), "'data.source' must be provided.")
  expect_error(build_webreport(data.source = "nofile.txt"), 'No file')
  expect_error(build_webreport(data.source = "test-data/ntweets.rds"),
               '\'data.source\' should be an SQLite database')
})

