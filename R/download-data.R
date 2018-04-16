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
  if (length(keyword > 1L)) {
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
