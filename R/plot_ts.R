#' Plot Weekly Time-Series Data For Online Platforms
#'
#' Uses locally stored data from Website, Facebook and Twitter profiles to plot
#' time-series for the rolling year in weekly time units.
#'
#' @param data An object of class \code{data.frame}
#' @param columnname A character string representing Date-Time values, or values
#' that can be coerced to Date-Time values.
#' @param colour A character string giving a valid R built-in colour as
#' represented in \code{\link[grDevices]{colors}}.
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
make_ts <- function(data, columnname, colour = 'blue') {
  stopifnot(inherits(data, 'data.frame'))

  if (length(columnname) > 1L)
    columnname <- columnname[1]
  if (!columnname %in% colnames(data))
    stop(sQuote(columnname), 'must be a column in "data"')

  # First pathway -> Twitter, others -> Facebook & website
  origin <- "1970-01-01"
  no.days <- 365
  dc <- data[[columnname]]
  if (is.numeric(dc) & all(dc > 1e9)) {
    tit <- "Twitter mentions"
    data[[columnname]] <-
      as.Date(as.POSIXct(dc, origin = origin))
    in.yr <- Sys.Date() - data[[columnname]] <= no.days
  }
  else {
    if (is.character(dc)) {
      tit <- 'Facebook posts'
      data[[columnname]] <- as.Date(dc)
      in.yr <- Sys.Date() - data[[columnname]] <= no.days
    }
    else if (is.numeric(dc)) {
      tit <- 'Website uploads'
      data[[columnname]] <-
        as.Date(dc, origin = origin)
      in.yr <- Sys.Date() - data[[columnname]] <= no.days
    }
    else
      stop('Does not handle columns of type', sQuote(typeof(dc)))
  }
  data <- data[which(in.yr),]
  this.wk <- as.numeric(format(Sys.Date(), "%V"))
  data$week <- as.numeric(format(data[[columnname]], "%V"))
  twts.by.wk <- data %>%
    group_by(week) %>%
    count() %>%
    mutate(lag = abs(week - this.wk)) %>%
    arrange(lag)

  plot(
    ts(twts.by.wk$n, start = 53 - this.wk),
    col = colour,
    lwd = 2,
    main = paste(tit, "over past 52 weeks")
  )
}
