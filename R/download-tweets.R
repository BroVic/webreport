globalVariables(
  c('consumer_key', 'consumer_secret',
    'access_token', 'access_secret')
  )


#' Download tweets and store in a local database
#'
#' @param db A local database file
#' @param keyword A character vector containing a search term
#'
#' @note This function currently only works with SQLite databases
#'
#' @export
download_tweets <- function(keyword, db)
{
  logon_to_twitter()
  update_db(keyword, db)
}














#' @importFrom twitteR setup_twitter_oauth
logon_to_twitter <- function() {
  keys <- system.file("keys/key.RData", package = "webreport")
  if (!nchar(keys)) {
    stop("OAuth credentials were not found")
  } else {
    load(keys)
  }
  setup_twitter_oauth(consumer_key, consumer_secret,
                      access_token, access_secret)
  cat("Authentication successful\n")
}










## Update a local database with Twitter data
#' @importFrom twitteR search_twitter_and_store
#' @importFrom twitteR register_sqlite_backend
update_db <- function(keyword, dBase) {
  register_sqlite_backend(dBase)
  cat("Updating database with NESREANigeria tweets... ")
  n <- search_twitter_and_store(keyword, paste0(keyword, "_tweets"))
  cat(sprintf(ngettext(
    n, "%d tweet added\n", "%d tweets added\n"
  ), n))
}
