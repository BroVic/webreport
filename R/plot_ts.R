# plot_ts.R

globalVariables(c('.', 'week'))

#' Plot Weekly Time-Series Data For Online Platforms
#'
#' Uses locally stored data from Website, Facebook and Twitter profiles to plot
#' time-series for the rolling year in weekly time units.
#'
#' @param data An object of class \code{data.frame}
#' @param platform A character string, with the name of the chosein online
#' platform. Options are Facebook, Twitter and Website.
#' @param base Logical; whether to create a base plot or a ggplot.
#' @param pt An integer value represeting the weight of the line. Defaults to 2L.
#'
#' @details The data used for this function i.e. the date variable, is expected
#' to be either a nummeric vector (as stored by Twitter and Website data) or as
#' a character vector (Facebook). The Facebook date elements are stored in the
#' POSIX-compatible \code{YYYY-MM-DDTHH:MM:SS+} format. Any other presentation
#' produce an error.
#'
#' @return Has no return value per se. It results in the production of a
#' time-series plot (either a base \code{graphics} or a
#' \code{\link[ggplot2]{ggplot}} type of plot.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom graphics plot
#' @importFrom stats ts
#'
#' @export
make_ts <- function(data, platform, base = TRUE, pt = 2L) {
  stopifnot(inherits(data, 'data.frame'))
  stopifnot(is.character(platform))

  ## Extract platform-specific values
  specs    <- .preparePlatformSpecifics(platform, data)
  data     <- specs$data
  date.col <- specs$date.column
  colour   <- specs$colour
  tt       <- specs$title.stub
  this.wk  <- as.numeric(format(Sys.Date(), "%V"))

  updates.by.wk <- data %>%
    filter(as.name(eval(date.col)) >= (Sys.Date() - 365)) %>%
    mutate(week = as.numeric(format(data[[date.col]], "%V"))) %>%
    group_by(week) %>%
    count() %>%
    mutate(lag = abs(week - this.wk)) %>%
    arrange(lag)

  tt <- paste(tt, "over past 52 weeks")
  if (base) {
    plot(
      ts(updates.by.wk$n, start = 53 - this.wk),
      lwd = pt,
      main = tt,
      col = colour
    )
  }
  else {
    print(ggplot(updates.by.wk, aes(week, n)) +
            geom_line(colour = colour, size = pt) +
            ggtitle(tt))
  }
}










## Prepares internal variables for 'make_ts' depending on the online platform
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
.preparePlatformSpecifics <- function(x, DATA)
{
  stopifnot(is.data.frame(DATA))
  stopifnot(exprs = {
    is.character(x)
    length(x) > 0
  })
  x <- tolower(x[1L])
  origin <- "1970-01-01"
  col <- c(twitter = 'lightblue', facebook = 'darkblue', website = 'darkgreen')
  if (x %in% 'twitter') {
    var <- 'created'
    DATA[[var]] <- DATA[[var]] %>%
      { as.Date(as.POSIXct(., origin = origin)) }
    tt <- "Twitter mentions"
    col <- col['twitter']
  }
  else if (x %in% 'facebook') {
    var <- 'created_time'
    DATA[[var]] <- as.Date(DATA[[var]])
    tt <- 'Facebook posts'
    col <- col['facebook']
  }
  else if (x %in% 'website') {
    var <- 'Date'
    DATA[[var]] <- as.Date(DATA[[var]], origin = origin)
    tt <- 'Website uploads'
    col <- col['website']
  }
  else
    stop(sQuote(x), ' is not a supported platform')
  list(data = DATA, date.column = var, title.stub = tt, colour = col)
}
