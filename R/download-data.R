#' download_all_data
#'
#' @description Download various web data from the internet
#' @param data.store An SQLite database
#' @param keyword A character vector containing a search term
#'
#' @export
download_all_data <- function(keyword, data.store)
{
  if (length(keyword > 1L))
    warning("'keyword' has more than one element and only the first was used.")
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
  if (identical(keyword, "nesrea") | identical(keyword, "nesreanigeria")) {
    cat(sprintf(beg, wb))
    download_website(data.store)
    cat(wb, end)
  } else {
    message(sprintf(
      "Web scraping for keyword '%s' is not yet supported.", keyword
    ))
  }
}
