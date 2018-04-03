globalVariables(c("created", "isRetweet"))
#'  collect_tweets
#'
#' Collects tweets to a maximum of 1,000
#' @param string A search term
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
  twt <- searchTwitter(string, n = 1000) %>%
    twListToDF()
}




#' display_twts
#'
#' Displays a density plot of tweets
#' @param x A data frame created from a list of Twitter JSON objects
#' @import ggplot2
#' @export
display_twts <- function(x)
{
  if (!is.data.frame(x))
    stop("x is not a data frame")
  tgt <- c("created", "isRetweet")
  if (!identical(match(tgt, colnames(x)), as.integer(c(5, 13))))
    stop("Not a valid tweet data frame.")
  if (!inherits(x$created[1], "POSIXct")& is.logical(x$isRetweet))
    stop("The data do not match the type required for the analysis.")
  plot <- ggplot(x, aes(created)) +
    geom_density(aes(fill = isRetweet), alpha = 0.7) +
    theme(legend.justification = c(1, 1),
          legend.position = c(1, 1)) +
    xlab("All tweets")
  plot
}













#' show_tweets_containing
#'
#' Search for and display tweet(s) containing a particular word
#'
#' @param word A character vector of strings that are being searched for
#'
#' @export
show_tweets_containing <- function(word)
  function(word = character(), df = data.frame()) {
    if (!is.character(word))
      stop("Expected a string as input")
    if (length(word > 1)) {
      word <- word[1]
      warning("First element of 'word' used; the rest was discarded.")
    }

    index <- grepl(word, df$text, fixed = TRUE)
    if (any(index)) {
      success <- df$text[index]
      print(success)
    } else {
      cat("Word not found")
    }
  }












#' compare_mentions
#'
#' Prints a proportions table of number of tweets for various search terms
#' @param x character vector of search terms
#' @param n max. number of tweets to download (default is 50)
#'
#' @importFrom twitteR searchTwitter
#'
#' @export
compare_mentions <- function(x, n = 50L) {
  if (!is.character(x))
    stop("'x' is not a character vector.")
  if (!is.atomic(x))
    stop("'x': Expected an atomic vector.")
  if (is.numeric(n))
    n <- as.integer(n)
  if (!is.integer(n))
    stop("'n' is not an integer type.")
  tweetData <- sapply(x, USE.NAMES = TRUE, function(term) {
    twList <- suppressWarnings(searchTwitter(term, n))
  })
  twtNum <- sapply(tweetData, USE.NAMES = TRUE, length)
  twtNum <- as.table(twtNum)
}


















#' chart_tweets
#'
#' Generates a barplot that compares the propotion of tweets with differnt
#' search terms, colours it and gives it appropriate labels
#' @param tbl A table; principally object returned by \code{compare_mentions}
#' @importFrom graphics barplot
#'
#' @export
chart_tweets <- function(tbl)
{
  if (!is.table(tbl))
    stop("Argument is not a table.")
  barplot(tbl, col = "brown", main = "Comparative barplot of tweets")
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
