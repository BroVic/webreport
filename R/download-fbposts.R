#' download_fb.R
#'
#' @param dBase A database file
#' @param keyword A character vector containing a search term
#'
#' @import RSQLite
#' @importFrom dplyr distinct
#' @importFrom Rfacebook getPage
#'
#' @export
download_fb <- function(keyword, dBase)
{
  ## TODO: Implement exception handling while DB is open
  cat("Connecting the database... ")
  sql.conn <- dbConnect(SQLite(), dBase)
  if (dbIsValid(sql.conn))
    cat("DONE\n")
  else
    stop("There was a problem connecting to the database\n")
  cat("Downloading Page posts from the Newsfeed\n")
  nesreaToken <- fetch_token()
  nesreaToken <- nesreaToken$token
  posts <-
    getPage(page = keyword,
            nesreaToken,
            n = 200,
            feed = TRUE)
  dbWriteTable(sql.conn, paste0(keyword, "_fbposts"), posts, overwrite = TRUE)
  cat("from Newsfeed were stored\n")
  store_post_details(keyword, sql.conn, posts)
  cat("Checking for and correcting duplications... ")
  tbls <- dbListTables(sql.conn)
  tbls <- tbls[grepl("fb", tbls)]
  sapply(tbls, function(Tb) {
    temp <- dbReadTable(sql.conn, Tb)
    temp <- distinct(temp)
    dbWriteTable(sql.conn, Tb, temp, overwrite = TRUE)
  })
  cat("DONE\n")
  cat("Disconnecting the database... ")
  dbDisconnect(sql.conn)
  if (!dbIsValid(sql.conn)) {
    cat("DONE\n")
  } else {
    warning("The database was not properly disconnected.")
  }
}
