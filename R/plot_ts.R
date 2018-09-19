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
#' @param ... Additional arguments passed to \code{plot}.
#'
#'
#' @return Has no return value per se. It results in the production of a
#' time-series plot.
#'
#' @importFrom stats ts
#'
#' @export
make_ts <- function(data, platform, ...) {
  stopifnot(inherits(data, 'data.frame'))
  stopifnot(is.character(platform))
  ps <- platformSpecs(data, platform)
  res <- prepare(ps,'NESREANigeria')
  tso <- ts(data = res,
            start = c(2016, 12),    # TODO: Change hard coding here!
            end = c(2018, 8),
            frequency = 12)
  .drawTimeSeries(tso, specs = ps)
}



#' @importFrom graphics plot
#' @importFrom stats window
.drawTimeSeries <- function(tsObj, startDate = -365L, base = TRUE, specs, ...)
{
  stopifnot(exprs = {
    inherits(tsObj, 'ts')
    is.numeric(startDate)
    is.logical(base)
    inherits(specs, 'platformSpecs')
  })
  if (base) {
    colour <- specs$colour
    legend <- 'All updates'
    if(is.matrix(tsObj) && ncol(tsObj) == 2) {
      colour <- c(colour, 'red')
      legend <- c(legend, 'NESREA')
    }
    plot(
      window(
        tsObj,
        start = c(.numericalDateElem('%Y', startDate),
                  .numericalDateElem('%m', startDate)),
        end = c(.numericalDateElem('%Y'),
                .numericalDateElem('%m'))
      ),
      plot.type = 'single',
      col = colour,
      lwd = 2,
      main = specs$title.stub,
      ylab = 'Posts'
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
.numericalDateElem <- function(placeholder, dif = 0) {
  stopifnot(exprs = {
    is.character(placeholder)
    grepl('^%[A-Za-z]{1}', placeholder)
  })
  if (!is.numeric(dif) & !is.integer(dif))
    stop("'dif' should be a number")
  if (abs(dif) > 365)
    warning('You have elected to use a time frame larger than 1 year')
  as.numeric(format(Sys.Date() + dif, placeholder))
}
