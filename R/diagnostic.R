# diagnostic.R

## The functions in this file are basically meant to ease in-project analyses
## e.g. loading of the data. This file is not part of the final build i.e.
## it will be added to '.Rbuildignore'.


## This operation is abstracted to avoid repetition. The path provided
## as an argument is specific to a particular computer
if (Sys.info()['nodename'] %in% 'SA-DG') {
  .dataB <- '../NESREA_social/data/nesreanigeria.db'
  if (!file.exists(.dataB))
    warning('The database', sQuote(basename(.dataB)), "was not found")
}

## Reads all the data
dat <- function()
{
  stopifnot(exists('.dataB', env = parent.env(environment())))
  provideInternalData(.dataB)
}


## The following functions are for quickly importing data for the
## various web or social media platforms stored in a local database
.getTwitterData <- function()
{
  d <- dat()
  d$tweets
}

.getFacebookPosts <- function()
{
  d <- dat()
  d$fbposts
}

.getFacebookLikes <- function()
{
  d <- dat()
  d$fblikes
}

.getFacebookComments <- function()
{
  d <- dat()
  d$fbcomments
}

.getWebNews <- function()
{
  d <- dat()
  d$webnews
}
