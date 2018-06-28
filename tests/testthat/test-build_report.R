# test-build_webreport.R

context("Report generation")

test_that("appropriate data are sourced", {
  expect_error(build_webreport())
  expect_error(build_webreport(), "'data.source' must be provided.")
  expect_error(build_webreport(data.source = "file.txt"))
  expect_error(build_webreport(data.source = "no-extension"))
  # expect_error(build_webreport("fakeFile.Rmd", "fakeDB.db", gui = FALSE),
  #              "file 'fake_path' does not exist")
})

test_that("altered fields from database are reprocessed", {
  logi <- c('favorited', 'truncated', 'isRetweet', 'retweeted')
  df <- readRDS("test-data/ntweets.rds")
  procDF <- process_stored_tweets(df)

  expect_error(process_stored_tweets(67))
  expect_error(process_stored_tweets(data.frame(
    c(1:3), c(T, F, T), c(letters[1:3]), c(rep(9)), stringsAsFactors = FALSE
  )))
  expect_error(process_stored_tweets(within(df, df <-
                                              as.logical(df$isRetweet))))
  expect_is(procDF, "data.frame")
  expect_equal(c(25, 16), dim(procDF))
  expect_true(all(logi %in% colnames(procDF)))
  expect_type(procDF$created, "double")
  expect_is(procDF$created, "POSIXct")
  for (i in seq_along(logi))
    expect_type(procDF[[logi[i]]], "logical")
})

test_that("emotional valence is computed", {
  expect_error(compute_emotional_valence(42))
  expect_error(compute_emotional_valence(logical(2)))
  expect_error(compute_emotional_valence(matrix(LETTERS, ncol = 13L)))
  expect_error(compute_emotional_valence(character()))
})

polList <- compute_emotional_valence(qdap::pres_debate_raw2012$dialogue)

test_that("polarity list is generated", {
  expect_is(polList, 'list')
  expect_is(polList[1], 'list')
  expect_is(polList[[1]], 'list')
  expect_is(polList[[1]], 'polarity')
  expect_is(polList[[1]]$all, 'data.frame')
  expect_is(polList[[1]]$group, 'data.frame')
})

wrdTable <- make_word_table(polList)

test_that("Word table is created", {
  expect_is(wrdTable, 'list')
  expect_equal(names(wrdTable), c('positiveWords', 'negativeWords'))
  expect_is(wrdTable$positiveWords, 'table')
  expect_is(wrdTable$negativeWords, 'table')
})

test_that("Social media network can be selected", {
  expect_error(choose_platform())
  expect_error(choose_platform(9))
  expect_error(choose_platform(TRUE))
  expect_equal(choose_platform('Twitter'), 1L)
  expect_equal(choose_platform('Facebook'), 2L)
  expect_error(choose_platform('otherNetwork'), 'not a supported social media')
})
