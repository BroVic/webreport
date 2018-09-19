# test-globals.R

context('Sentiment analytics')

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


context('Platform selection')

test_that("Social media network can be selected", {
  expect_error(choosePlatform())
  expect_error(choosePlatform(9))
  expect_error(choosePlatform(TRUE))
  expect_equal(choosePlatform('Twitter'), 1L)
  expect_equal(choosePlatform('Facebook'), 2L)
  expect_error(choosePlatform('otherNetwork'), 'not a supported social media')
})
