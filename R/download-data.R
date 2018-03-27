#' download_all_data
#'
#' @description Download various web data from the internet
#' @param data.store An SQLite database
#' @export
download_all_data <- function(data.store)
{
  tw <- "Twitter"
  fb <- "Facebook"
  wb <- "Website"
  beg <- "Starting %s downloads\n"
  end <- "downloads completed\n\n"
  cat(sprintf(beg, tw))
  download_tweets(data.store)
  cat(tw, end)
  cat(sprintf(beg, fb))
  download_fb(data.store)
  cat(fb, end)
  cat(sprintf(beg, wb))
  download_website(data.store)
  cat(wb, end)
}
