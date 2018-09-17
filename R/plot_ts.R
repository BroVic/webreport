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
  ps <- platformSpecs(data, platform)
  res <- prepare(ps,'NESREANigeria')
  tsObj <-
    ts(
      data = res,
      start = c(2016, 12),
      end = c(2018, 8),
      frequency = 12
    )

  if (base) {
    startDate <- -365
    plot(
      window(
        tsObj,
        start = c(
          .numericalDateElem('%Y', startDate),
          .numericalDateElem('%m', startDate)
        ),
        end = c(.numericalDateElem('%Y'), .numericalDateElem('%m'))
      ),
      plot.type = 'single',
      col = c(ps$colour, 'red'),
      lwd = 2,
      main = ps$title.stub
    )
  }
  else {
    message("Other plotting formats not yet implemented")
    # print(ggplot(updates.by.wk, aes(week, n)) +
    #         geom_line(colour = colour, size = pt) +
    #         ggtitle(ps$title.stub))
  }
}











## Gets the numerical value for a given element of a Date object
.numericalDateElem <- function(placeholder, dif = 0) {
  stopifnot(is.character(placeholder), grepl('^%[A-Za-z]{1}', placeholder))
  if (!is.numeric(dif) & !is.integer(dif))
    stop("'dif' should be a number")
  if (abs(dif) > 365)
    warning('You have elected to pick a time frame larger than 1 year')
  as.numeric(format(Sys.Date() + dif, placeholder))
}
