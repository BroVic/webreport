globalVariables(c("created", "isRetweet"))
#' Collect Some Tweets
#'
#' Collects tweets containing a particular term, to a maximum of 1,000.
#'
#' @param string A character vector of length 1 of a search term
#'
#' @importFrom twitteR searchTwitter
#' @importFrom twitteR twListToDF
#' @importFrom dplyr %>%
#'
#' @export
collect_tweets <- function(string)
{
  if (!is.character(string))
    stop("'string' must be a character vector.")
  if (length(string) > 1) {
    string <- string[1]
    warning(sQuote(string), "was used for the search and other terms dropped.")
  }
  num.letters <- nchar(string)
  if ( num.letters< 3 || num.letters > 20)
    stop("A term of between 3 and 20 characters is required.")
  searchTwitter(string, n = 1000) %>%
    twListToDF()
}




# Visualisation of Twitter Data
#
# Displays a density plot of tweets.
#
# @param x A data frame created from a list of Twitter JSON objects
# @import ggplot2
# @export
# display_twts <- function(x)
# {
#   if (!is.data.frame(x))
#     stop("x is not a data frame")
#   tgt <- c("created", "isRetweet")
#   if (!identical(match(tgt, colnames(x)), as.integer(c(5, 13))))
#     stop("Not a valid tweet data frame.")
#   if (!inherits(x$created[1], "POSIXct")& is.logical(x$isRetweet))
#     stop("The data do not match the type required for the analysis.")
#   plot <- ggplot(x, aes(created)) +
#     geom_density(aes(fill = isRetweet), alpha = 0.7) +
#     theme(legend.justification = c(1, 1),
#           legend.position = c(1, 1)) +
#     xlab("All tweets")
#   plot
# }













# Show selected Tweets
#
# Search for and display tweet(s) containing a particular word
#
# @param word A character vector of strings that are being searched for
#
# @export
# show_tweets_containing <- function(word)
#   function(word = character(), df = data.frame()) {
#     if (!is.character(word))
#       stop("Expected a string as input")
#     if (length(word > 1)) {
#       word <- word[1]
#       warning("First element of 'word' used; the rest was discarded.")
#     }
#
#     index <- grepl(word, df$text, fixed = TRUE)
#     if (any(index)) {
#       success <- df$text[index]
#       print(success)
#     } else {
#       cat("Word not found")
#     }
#   }
#











#' Comparing Twitter Mentions
#'
#' Prints a proportions table of number of tweets for various search terms.
#'
#' @param terms Character vector of Twitter handles of length > 1
#' @param n Maximum number of tweets to download (default is 500)
#' @param from Tweets from a given data in the format YYYY-MM-DD
#' @param to Tweets up to a given data in the format YYYY-MM-DD
#'
#' @importFrom graphics barplot
#' @importFrom twitteR searchTwitter
#' @importFrom twitteR setup_twitter_oauth
#'
#' @export
compare_mentions <- function(terms, n = 500L, from = NULL, to = NULL) {

  ## TODO: introduce a check that will only enable actual Twitter
  ##       handles to be processed.
  if (!is.atomic(terms))
    stop("'terms': Expected an atomic vector")
  if (!is.character(terms))
    stop("'terms' is not a character vector")
  if (!all(grepl("^@[[:alnum:]]+$", terms)))
    stop("The analysis is restricted to Twitter handles beginning with '@'")
  if (length(terms) < 2)
    stop("Nothing to compare. Pass at least 2 handles")
  if (!is.numeric(n))
    stop("'n' is not numeric")
  else if(!is.integer(n))
    n <- as.integer(n)
  if (!is.null(from) || !is.null(to)) {
    cmb <- c(from, to)
    if (!any(grepl("^[2][0][0-1][0-9]-[0-1][0-9]-[0-3][0-9]$", cmb)))
      stop("Illegal date format; expected 'YYYY-MM-DD'")
  }
  if (is.null(from) & !is.null(to))
    stop("'to' cannot be passed when 'from' is NULL")
  e <- new.env()
  load(system.file("keys/key.RData", package = "webreport"), envir = e)
  setup_twitter_oauth(e$consumer_key, e$consumer_secret,
                      e$access_token, e$access_secret)
  tweetData <- sapply(terms, USE.NAMES = TRUE, function(x) {
    suppressWarnings(list(searchTwitter(x, n, since = from, until = to)))
  })
  twtNum <- sapply(tweetData, FUN = length)
  print(as.table(twtNum))
  barplot(twtNum, col = "brown", main = "Comparative barplot of mentions")
}












# When the database is queried with \code{RSQLIte::dbReadTable()},
# the data types of some of the columns do not conform to the desired state.
# This function changes the column with dates to POSIX format, while
# appropriate colums of the data frame are converted to \code{logical}.
process_stored_tweets <- local({
  function(data)
  {
    stopifnot(inherits(data, 'data.frame'))
    if (!identical(ncol(data), 16L))
      stop("Number of columns is ", ncol(data))
    sapply(
      c('favorited', 'truncated', 'isRetweet', 'retweeted'),
      function(var) {
        data[[var]] <<- as.logical(data[[var]])
      })
    data$created <- as.POSIXct(data$created, origin = '1970-01-01')
    invisible(data)
  }
})
