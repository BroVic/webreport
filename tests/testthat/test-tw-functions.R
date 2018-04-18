## test-tw-functions.R

context("Rapid checks")

test_that("Twitter mentions can be compared", {
  hnd1 <- "@nesreanigeria"
  hnd2 <- "@nosdranigeria"
  nm1 <- "nesreanigeria"
  nm2 <- "nosdranigeria"

  expect_error(compare_mentions())
  expect_error(compare_mentions(c(hnd1, nm2)))
  expect_error(compare_mentions(c(hnd2, nm1)))
  expect_error(compare_mentions(c(nm1, nm2)),
               "The analysis is restricted to Twitter handles ")
  expect_error(compare_mentions(hnd1),
               "Nothing to compare. Pass at least 2 handles")
  expect_error(compare_mentions(5),
               "'terms' is not a character vector")
  expect_error(compare_mentions(matrix(hnd1, hnd2)))
  expect_error(compare_mentions(data.frame(hnd1, hnd2)))
  expect_error(compare_mentions(c(hnd1, hnd2), n = "twenty"),
               "'n' is not numeric")
  expect_error(compare_mentions(c(hnd1, hnd2), from = "5 April 2018"),
               "Illegal date format; expected 'YYYY-MM-DD'")
  expect_error(compare_mentions(c(hnd1, hnd2), to = "5 April 2018"),
              "Illegal date format; expected 'YYYY-MM-DD'")
  expect_error(compare_mentions(c(hnd1, hnd2), to = as.character(Sys.Date())),
               "'to' cannot be passed when 'from' is NULL")
})
