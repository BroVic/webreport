globalVariables(
  c('consumer_key', 'consumer_secret',
    'access_token', 'access_secret')
  )
#' Download tweets and store in a local database
#'
#' @param db A local database file
#'
#' @note This function currently only works with SQLite databases
#'
#' @export
download_tweets <- function(db)
{
  logon_to_twitter()
  update_db(db)
}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' logon_to_twitter
#'
#' @description Logon to Twitter API using Oauth credentials
#' @importFrom twitteR setup_twitter_oauth
logon_to_twitter <- function() {
  keys <- system.file("keys", "keys.RData", "webreport")
  if (!file.exists(keys)) {
    warning("You must supply OAuth credentials to proceed")
  } else {
    load(keys, envir = globalenv())
  }
  setup_twitter_oauth(consumer_key, consumer_secret,
                      access_token, access_secret)
  cat("Authentication successful\n")
  rm(consumer_key,
     consumer_secret,
     access_token,
     access_secret,
     envir = globalenv())
}
#'
#'
#'
#'
#'
#'
#' Update a local database with Twitter data
#'
#' @param dBase A database file for storage of downloaded tweets.
#'
#' @importFrom twitteR search_twitter_and_store
#' @importFrom twitteR register_sqlite_backend
#' @export
update_db <- function(dBase) {
  register_sqlite_backend(dBase)
  cat("Updating database with NESREANigeria tweets... ")
  n <- search_twitter_and_store("nesreanigeria", "nesreanigeria_tweets")
  cat(sprintf(ngettext(
    n, "%d tweet added\n", "%d tweets added\n"
  ), n))
}
