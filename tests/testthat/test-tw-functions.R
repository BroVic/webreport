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



# ====

context('Local data retrieval')

test_that('Tweet data is type-checked and transformed where appropriate', {
  twts <- readRDS('test-data/ntweets.rds')
  val <- process_stored_tweets(twts)

  expect_is(val, 'data.frame')
  expect_type(val, 'list')
  expect_error(process_stored_tweets(),
               'argument "data" is missing, with no default')
  expect_error(process_stored_tweets(999))
  expect_error(process_stored_tweets(mtcars),
               'Number of columns is 11')
  ## TODO: Add test case where columns == 16 but data are incompatible
  expect_error(process_stored_tweets("A string."),
               'inherits(data, "data.frame") is not TRUE',
               fixed = TRUE)
})
