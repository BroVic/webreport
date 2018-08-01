#' Plot Weekly Time-Series Data For Online Platforms
#'
#' Uses locally stored data from Website, Facebook and Twitter profiles to plot
#' time-series for the rolling year in weekly time units.
#'
#' @param data An object of class \code{data.frame}
#' @param date.col A character string representing Date-Time values, or values
#' that can be coerced to Date-Time values.
#' @param base Logical; whether to create a base plot or a ggplot.
#' @param colour A character string giving a valid R built-in colour as
#' represented in \code{\link[grDevices]{colors}}.
#' @param pt An integer value represeting the weight of the line. Defaults to 2L.
#'
#' @details The data used for this function i.e. the date variable, is expected
#' to be either a nummeric vector (as stored by Twitter and Website data) or as
#' a character vector (Facebook). The Facebook date elements are stored in the
#' POSIX-compatible \code{YYYY-MM-DDTHH:MM:SS+} format. Any other presentation
#' produce an error.
#'
#' @return A base time-series plot.
#'
#' @import dplyr
#' @export
make_ts <- function(data, date.col, base = TRUE, colour, pt = 2L) {
  stopifnot(inherits(data, 'data.frame'))

  if (length(date.col) > 1L)
    date.col <- date.col[1]
  if (!date.col %in% colnames(data))
    stop(sQuote(date.col), 'must be a column in "data"')

  # First pathway -> Twitter, others -> Facebook & website
  origin <- "1970-01-01"
  no.days <- 365
  dc <- data[[date.col]]
  if (is.numeric(dc) & all(dc > 1e9)) {
    tit <- "Twitter mentions"
    data[[date.col]] <-
      as.Date(as.POSIXct(dc, origin = origin))
    in.yr <- Sys.Date() - data[[date.col]] <= no.days
  }
  else {
    if (is.character(dc)) {
      tit <- 'Facebook posts'
      data[[date.col]] <- as.Date(dc)
      in.yr <- Sys.Date() - data[[date.col]] <= no.days
    }
    else if (is.numeric(dc)) {
      tit <- 'Website uploads'
      data[[date.col]] <-
        as.Date(dc, origin = origin)
      in.yr <- Sys.Date() - data[[date.col]] <= no.days
    }
    else
      stop('Does not handle columns of type', sQuote(typeof(dc)))
  }
  data <- data[which(in.yr),]
  this.wk <- as.numeric(format(Sys.Date(), "%V"))
  data$week <- as.numeric(format(data[[date.col]], "%V"))
  updates.by.wk <- data %>%
    group_by(week) %>%
    count() %>%
    mutate(lag = abs(week - this.wk)) %>%
    arrange(lag)

  tit <- paste(tit, "over past 52 weeks")
  if (base) {
    ## base plot
    plot(ts(updates.by.wk$n, start = 53 - this.wk),
         lwd = pt,
         main = tit,
         col = colour)
  }
  else {
    ## ggplot
    ggplot(updates.by.wk, aes(week, n)) +
      geom_line(col = colour, size = pt) +
      ggtitle(tit)
  }
}
