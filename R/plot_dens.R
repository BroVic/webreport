# plot_dens.R

#' Draw a density plot of social data
#'
#' Draw a Kernel density plot from previously stored social media data
#'
#' @param data An object of class \code{data.frame}
#' @param platform A character string representing one of 2 social media sites
#' -- \href{http://twitter.com/}{Twitter} or
#' \href{htpp://facebook.com/}{Facebook}
#'
#' @return A \code{ggplotObj} object which upon printing draws a density plot.
#' @import ggplot2
#'
#' @export
dens_plot <- function(data, platform)
{
  choice <- choosePlatform(site = platform)
  type <- c("tweets", "comments")
  var <- c("created", "created_time")
  hue <- c("lightblue", "darkblue")
  title <- paste("Proportion of", type[choice])

  print(
    ggplot(data, aes_string(var[choice])) +
      geom_density(fill = hue[choice], alpha = 0.4) +
      theme(
        legend.justification = c(1, 1),
        legend.position = c(1, 1)
      ) +
      ggtitle(title) +
      xlab("Date")
  )
}
