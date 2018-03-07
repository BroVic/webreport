# test-build_report.R

context("Building of the report useing template")
test_that("Report can be built", {
  expect_error(build_report())
  expect_error(build_report(), "argument \"file\" is missing, with no default")
  # expect_error(build_report(9), "is.character(file) is not TRUE")
  expect_error(build_report("fake_path"), "file 'fake_path' does not exist")
})
