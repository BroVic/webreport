# diagnostic.R

## This operation is abstracted to avoid repetition. The path provided
## as an argument is specific to a particular computer
dat <- function()
{
  provideInternalData('../NESREA_social/data/nesreanigeria.db')
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
