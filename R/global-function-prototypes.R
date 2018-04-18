# Calclates the polarity for each text field
## This operation ought to generate series of warnings because of multiple
## punctuations that expectedly occur in the text, but this has been suppressed
#' @importFrom qdap polarity
compute_emotional_valence <- function(text.var) {
  suppressWarnings(
    lapply(text.var, function(txt) {
      txt <- gsub("(\\.|!|\\?)+\\s+|(\\++)", " ", txt)
      txt <- gsub(" http[^[:blank:]]+", "", txt)
      polarity(txt)
    })
  )
}






#' @importFrom graphics par
#' @importFrom graphics dotchart
#' @importFrom graphics mtext
visualise_pol_diff <- function(pol.list) {
  pol.tab <- make_word_table(pol.list)
  oldpar <- par()
  par(mfrow = c(1, 2))
  invisible(
    lapply(1:2, function(i) {
      suppressWarnings(dotchart(sort(pol.tab[[i]]), cex = .8))
      mtext(names(pol.tab)[i])
    })
  )
  suppressWarnings(par(oldpar))
}

















#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics layout
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics text
#' @importFrom tm removeWords
#' @importFrom tm TermDocumentMatrix
#' @importFrom wordcloud comparison.cloud
generate_wordcloud <- function(data, pol.list, site)
{
  pol.tab <- make_word_table(pol.list)
  polSplit <- split(data, sign(data$emotionalValence))
  picked <- choose_platform(site)
  var <- c("text", "message")
  var <- var[picked]
  if (length(polSplit) != 3) {
    cat("Insufficient data to render the wordcloud\n")
  }
  else {
    polText <- sapply(polSplit, function(df) {
      text <- paste(tolower(df[, var]), collapse = ' ')
      text <- gsub(' http|@)[^[:blank:]]+', '', text)
      text <- gsub('[[:punct:]]', '', text)
    })
    polText <-
      structure(polText, names = c('negative', 'neutral', 'positive'))
    polText['negative'] <-
      removeWords(polText['negative'], names(pol.tab$negativeWords))
    polText['positive'] <-
      removeWords(polText['positive'], names(pol.tab$positiveWords))
    corp <- make_corpus(polText)
    col3 <- brewer.pal(3, 'Paired')
    layout(mat = matrix(c(1, 2), nrow = 2), heights = c(1, 4))
    par(mar = rep(0, 4))
    plot.new()
    text(x = 0.5, y = 0.5,
         sprintf("Comparison Cloud of Words from %s Data", site))
    suppressWarnings(
      comparison.cloud(as.matrix(TermDocumentMatrix(corp)),
                       max.words = 150,
                       min.freq = 1,
                       random.order = FALSE,
                       rot.per = 0,
                       colors = col3,
                       vfont = c("sans serif", "plain")))
  }
}


# Makes a table of words on eiher side of the emotional spectrum i.e.
# positive/negative
#' @importFrom tm stripWhitespace
make_word_table <- function(pol.list) {
  wordsTab <- sapply(pol.list, function(p) {
    words <-
      c(positiveWords = paste(p$all$pos.words[[1]], collapse = ' '),
        negativeWords = paste(p$all$neg.words[[1]], collapse = ' '))
    gsub('-', '', words)
  })
  wordsTab <- apply(wordsTab, MARGIN = 1, FUN = paste, collapse = ' ')
  wordsTab <- stripWhitespace(wordsTab)
  wordsTab <- strsplit(wordsTab, ' ')
  wordsTab <- sapply(wordsTab, table, simplify = FALSE)
}









# Selects the appropriate social media platform being analysed. Returns an
# integer value, 1 for Twitter and 2 for Facebook, which is used internally
# for indexing other relevant functions.
choose_platform <- function(site)
{
  stopifnot(is.character(site))
  if(identical(tolower(site), "twitter")) return(1L)
  else if (identical(tolower(site), "facebook")) return(2L)
  else stop("Argument 'site' is not a supported social media platform.")
}







# Makes a volatile corpus and prepares text
#' @import tm
#' @importFrom dplyr %>%
make_corpus <- function(GText, stem = TRUE) {
  corp <- VCorpus(VectorSource(GText)) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english"))
  if(stem)
    corp <- tm_map(corp, stemDocument)

  names(corp) <- names(GText)
  corp
}









## Draw a density plot of social data
#' @import ggplot2
plain_dens_plot <- function(data, platform)
{
  choice <- choose_platform(site = platform)
  type <- c("tweets", "comments")
  var <- c("created", "created_time")
  hue <- c("red", "darkblue")
  title <- paste("Proportion of", type[choice])
  gg <- ggplot(data, aes_string(var[choice])) +
    geom_density(fill = hue[choice], alpha = 0.4) +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
    ggtitle(title) +
    xlab("Date")
  gg
}




# Returns a message that matches a particular metric
# (used only inside the body text)
#' @importFrom dplyr select
return_text <- function(df, metric) {
  column <- unlist(select(df, match(metric, colnames(df))))
  df$message[match(max(column), column)]
}





# Removes UNICODE characters that are not readable to humans e.g. emojis
## Note that this has only been tested on an English-based locale.
#' @importFrom stringr str_trim
remove_nonreadables <- function(string = NULL) {
  if (is.null(string))
    warning("No text data were available for reading.")
  nu.str <- gsub("[^[:graph:]]", " ", string)
  str_trim(nu.str)
}
