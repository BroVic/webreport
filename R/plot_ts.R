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
#' @importFrom stats ts
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
  tso <- ts(data = mat,
            start = c(begYr, begMth),
            frequency = 12L)
  .drawTimeSeries(tso, specs = ps)
}



#' @importFrom graphics plot
#' @importFrom stats window
.drawTimeSeries <- function(tsObj, base = TRUE, specs, ...)
{
  stopifnot(exprs = {
    inherits(tsObj, 'ts')
    is.logical(base)
    inherits(specs, 'platformSpecs')
  })
  if (base) {
    colour <- specs$colour
    legend <- 'All updates'
    if (is.matrix(tsObj) && ncol(tsObj) == 2) {
      colour <- c(colour, 'red')
      legend <- c(legend, 'NESREA')
    }
    plot(
      tsObj,
      plot.type = 'single',
      col = colour,
      lwd = 2,
      main = specs$title.stub,
      ylab = 'Posts',
      ylim = c(0, max(tsObj) + 2)
    )
    legend('topright', legend = legend, fill = colour)
  }
  else {
    message("Other plotting formats not yet implemented")
    # print(ggplot(updates.by.wk, aes(week, n)) +
    #         geom_line(colour = colour, size = pt) +
    #         ggtitle(ps$title.stub))
  }
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
