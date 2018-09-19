#' Download Website Data
#'
#' Downloads the webpage as an XML document and then makes a
#' dataframe from vectors of text scraped from parts of the page via CSS
#' selectors.
#'
#' @param database A database file for storing downloaded web data
#'
#' @import RSQLite
#' @import hellno
#' @import rvest
#' @importFrom dplyr distinct
#' @importFrom stringr str_trim
#' @importFrom xml2 read_html
#'
#' @export
download_website <- function(database)
{
  url <- "http://www.nesrea.gov.ng/news/"
  news <- read_html(url)
  cat("URL:", url, "\n")
  headers <-
    scrape_items(page = news, ".entry-header a", verbose = TRUE)
  descr <-
    scrape_items(page = news, ".short-description p", verbose = TRUE)
  descr <- descr[descr != ""]

  date <- scrape_items(page = news, ".sponsors", verbose = TRUE)
  date <- gsub("([[:digit:]])(st|rd|nd|th)", "\\1", date)
  date <- str_trim(date)
  date <- strptime(date, format = "%B %d, %Y")
  date <- as.Date(date)

  df <- data.frame(Title = headers, Description = descr, Date = date)

  cat("Connecting the database.... ")
  db <- dbConnect(SQLite(), database)
  cat("DONE\nStore website data....")
  dbWriteTable(db, "nesreanigeria_webnews", df, overwrite = TRUE)

  # TODO: Provide storage for blogs
  cat("DONE\nChecking for and correcting duplications... ")
  tmp <- dbReadTable(db, "nesreanigeria_webnews")
  tmp <- distinct(tmp)
  dbWriteTable(db, "nesreanigeria_webnews", tmp, overwrite = TRUE)
  cat("DONE\n")

  cat("Disconnecting the database...")
  dbDisconnect(db)
  if (dbIsValid(db)) {
    cat("Database was not disconnected.")
  }
  else
    cat("DONE\n")
}















#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
scrape_items <- function(page, selector, verbose = FALSE) {
  if (verbose)
    cat("Scraping data linked to the", sQuote(selector), "CSS selector... ")

  txt <- html_nodes(page, selector)
  txt <- html_text(txt)

  if (!length(txt)) {
    if (verbose)
      cat("NO DATA")
    warning("It is likely that a wrong CSS selector was used")
  }
  else {
    if (verbose)
      cat("Done\n")
    txt
  }
}
