#' download_fb.R
#'
#' @param dBase A database file
#' @import RSQLite
#' @importFrom dplyr distinct
#' @importFrom Rfacebook getPage
#'
#' @export
download_fb <- function(dBase)
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
    getPage(page = "nesreanigeria",
            nesreaToken,
            n = 200,
            feed = TRUE)
  dbWriteTable(sql.conn, "nesreanigeria_fbposts", posts, overwrite = TRUE)
  cat("from Newsfeed were stored\n")
  store_post_details(sql.conn, posts)
  cat("Checking for and correcting duplications... ")
  tbls <- dbListTables(sql.conn)
  tbls <- subset(grepl("fb", tbls))
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
    rm(sql.conn)
  } else {
    warning("The database could not be properly disconnected.")
  }
}
