# test-build_webreport.R

context("Report generation")

test_that("Report can be built", {
  expect_error(build_webreport())
  expect_error(build_webreport(), "'data.source' must be provided.")
  expect_error(build_webreport(data.source = "file.txt"))
  expect_error(build_webreport(data.source = "no-extension"))
  # expect_error(build_webreport("fakeFile.Rmd", "fakeDB.db", gui = FALSE),
  #              "file 'fake_path' does not exist")
})

polList <- compute_emotional_valence(qdap::pres_debate_raw2012$dialogue)

test_that("Polarity list is generated", {
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
