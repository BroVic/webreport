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
#' @param startDate A \code{Date} object of length 1. Defaults to 1 year back.
#' @param ... Additional arguments passed to \code{plot}.
#'
#'
#' @return Has no return value per se. It results in the production of a
#' time-series plot.
#'
#' @importFrom zoo zoo
#'
#' @export
make_ts <- function(data, platform, startDate = Sys.Date() - 365, ...) {
  stopifnot(exprs = {
    inherits(data, 'data.frame')
    inherits(startDate, 'Date')
    is.character(platform)
  })
  ps <- platformSpecs(data, platform)
  mat <- prepare(ps, 'NESREANigeria')
  begYr <- .numericalDateElem(startDate, '%Y')
  begMth <- .numericalDateElem(startDate, '%m')
  z <- zoo(mat, startDate + 1:365)
  .drawTimeSeries(z, specs = ps)
}


## Actually draws the time series plot.
## Receives a time series object as well as the
## characteristics applied to specific online platforms
## The 'base' arguments was originally intended to
## provide options for choosing between base and ggplot2
## graphics. This arguments is currently unimplemented.
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom stats window
.drawTimeSeries <- function(zObj, specs, ...)
{
  stopifnot(exprs = {
    inherits(zObj, 'zoo')
    inherits(specs, 'platformSpecs')
  })
  colour <- specs$colour
  legend <- 'All updates'
  if (is.matrix(zObj) && ncol(zObj) == 2) {
    colour <- c(colour, 'red')
    legend <- c(legend, 'NESREA')
  }
  plot(
    zObj,
    plot.type = 'single',
    type = 'n',
    main = specs$title.stub,
    ylab = 'Posts',
    ylim = c(0, max(zObj) + 5)
  )
  points(zObj, pch = 20, col = 'red')
  lines(zObj, col = colour, lwd = 2)
}






## Returns the numerical value for a given element of a Date object
.numericalDateElem <- function(date, placeholder) {
  stopifnot(exprs = {
    is.character(placeholder)
    grepl('^%[A-Za-z]{1}', placeholder)
  })
  stopifnot(inherits(date, 'Date'))
  if (abs(Sys.Date() - date) > 365)
    warning('You have elected to use a time-frame larger than 1 year')
  as.numeric(format(date, placeholder))
}
