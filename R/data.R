# data.R
# General functions related to the data relevant to the package

#' Download Data in General
#'
#' Downloads data from all the relevant website and social media platforms.
#'
#' @param data.store An SQLite database
#' @param keyword A character vector containing a search term
#'
#' @export
download_all_data <- function(keyword, data.store)
{
  if (length(keyword) > 1L) {
    keyword <- keyword[1]
    warning("'keyword' has more than one element and only the first was used.")
  }
  tw <- "Twitter"
  fb <- "Facebook"
  wb <- "Website"
  beg <- "Starting %s downloads\n"
  end <- "downloads completed\n\n"
  cat(sprintf(beg, tw))
  download_tweets(keyword, data.store)
  cat(tw, end)
  cat(sprintf(beg, fb))
  download_fb(keyword, data.store)
  cat(fb, end)

  ## Web scraping is specific to NESREA website
  if (identical(tolower(keyword), "nesrea") |
      identical(tolower(keyword), "nesreanigeria")) {
    cat(sprintf(beg, wb))
    download_website(data.store)
    cat(wb, end)
  } else {
    message(sprintf(
      "Web scraping for keyword '%s' is not yet supported.", keyword
    ))
  }
}





#' Display Datasets
#'
#' Present the data for inspection
#'
#' @param dbfile A path to a database file.
#'
#' @details This function is a enables the display of all the tables in the
#' database in the spreadsheet-like format prrovided by
#' \code{\link[utils]{View}}.
#'
#' @import utils
#' @export
show_datasets <- function(dbfile)
{
  if (!file.exists(dbfile))
    stop('File', sQuote(dbfile), 'does not exist')
  dt <- provideInternalData(dbfile)

  ## Iterate through list of data frames
  for(i in seq_along(dt)) {
    df <- dt[[i]]
    name <- names(dt)[i]
    if (is.null(df) || !nrow(df))
      warning(name, 'has no data')
    View(df, title = name)
  }
}
