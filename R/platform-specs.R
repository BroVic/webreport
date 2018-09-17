# platform-specs.R

# TODO: Introduce inheritance: SocialMediaData ----> Platforms

## Constructor for an (internal) S3 object that contains additional
## details specific to a given online or social media platform
platformSpecs <- function(DATA, x)
{
  stopifnot(inherits(DATA, 'data.frame'))
  stopifnot(exprs = {
    is.character(x)
    length(x) > 0
  })
  x <- tolower(x[1])
  origin <- "1970-01-01"
  if (x %in% 'twitter') {
    var <- 'created'
    DATA[[var]] <- as.Date(as.POSIXct(DATA[[var]], origin = origin))
    tt <- "Twitter mentions"
    col <- 'lightblue'
  }
  else if (x %in% 'facebook') {
    var <- 'created_time'
    DATA[[var]] <- as.Date(DATA[[var]])
    tt <- 'Facebook posts'
    col <- 'darkblue'
  }
  else if (x %in% 'website') {
    var <- 'Date'
    DATA[[var]] <- as.Date(DATA[[var]], origin = origin)
    tt <- 'Website uploads'
    col <- 'darkgreen'
  }
  else
    stop(sQuote(x), ' is not a supported platform')
  structure(list(
    name = tools::toTitleCase(x),
    data = DATA,
    date.colName = var,
    title.stub = tt,
    colour = col
  ),
  class = 'platformSpecs')
}






## S3 method that processes a data frame with social media data to
## an integer matrix of update counts carried per day.
## To be used for time-series analysis.
#' @import dplyr
prepare.platformSpecs <- function(x, sender) {
  stopifnot(is.character(sender))
  sender <- sender[1]
  dc <- x$date.colName
  data <- x$data
  isTwitter <- tolower(x$name) == 'twitter'
  if (isTwitter) {
    data <- data %>% mutate(bySender = screenName == sender)
    # TODO: Create cases for Facebook & Website (no, classes!)

    ## Aggregate the number of updates of 'sender'
    ## by the date and extract the column
    updatesBySender <- data %>%
      rename(day = dc) %>%
      group_by(day) %>%
      summarise(bySender = sum(bySender)) %>%
      select(bySender)
  }

  ## Find days with zero status updates
  ## which would be missing from the
  ## original data frame
  Zeros <- 0L
  zeroUpdateDays <-
    data[[dc]] %>%
    { seq(min(.), max(.), by = 'day') } %>%
    base::setdiff(data[[dc]]) %>%
    as_tibble() %>%
    rename(day = value) %>%
    mutate(n = Zeros) %>%
    { .['day'] <- as.Date(.$day, origin ='1970-01-01') } %>%
    as_tibble()
  if (isTwitter)
    zeroUpdateDays <- zeroUpdateDays %>% mutate(bySender = Zeros)
  data <- data %>%
    rename(day = dc) %>%
    group_by(day) %>%
    count()
  if (isTwitter)
    data <- data %>% bind_cols(updatesBySender)
  data %>%
    bind_rows(zeroUpdateDays) %>%
    arrange(day) %>%
    as.data.frame() %>%
    rename(allUpdates = n) %>%
    select(-day) %>%
    data.matrix() %>%
    invisible()
}
