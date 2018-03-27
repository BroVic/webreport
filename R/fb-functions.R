globalVariables(c("value", "end_time"))
#' Collect data on Insights from a Facebook Page.
#'
#' @description Retrieves and preprocesses insights from NESREA Facebook Page
#'
#' @param type The type of Facebook insight being queried.
#' @note This is a wrapper for \code{Rfacebook::getInsights}
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom Rfacebook getInsights
#' @export
choose_insight <-
  function(type = c("page_fan_adds", "page_fan_removes", "page_views_login",
                    "page_views_logout", "page_views", "page_story_adds",
                    "page_impressions", "page_posts_impressions",
                    "page_consumptions", "post_consumptions_by_type",
                    "page_fans_country"))
{
  NESREA_page_id <- use_pg_id()
  API_version <- get_api_version()
  nesreaToken <- fetch_token()
  nesreaToken <- nesreaToken$token
  result <-
    getInsights(object_id = NESREA_page_id, token = nesreaToken,
                        metric = type, version = API_version) %>%
    select(value:end_time)
  result$end_time <- substr(result$end_time, start = 1,
                            stop = regexpr("T", result$end_time) - 1)
  result
}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' Prepare Downloaded Facebook Data for further Processing.
#'
#' @param df An object of class \code{data.frame}, specifically downloaded
#' via that Facebook API.
#'
#' @description Processes Facebook data prior to analysis
prepare_data <- function(df)
{
  cnames <-
    c("from_id", "from_name", "message", "created_time", "type", "link",
      "id", "story", "likes_count", "comments_count", "shares_count")
  if (!identical(colnames(df), cnames))
    stop("Loaded data are not compatible with this function")
  df$type <- as.factor(df$type)
  df$created_time <- gsub("T", " ", df$created_time)
  df$created_time <- gsub("\\+", " \\+", df$created_time)
  df$created_time <- as.POSIXct(df$created_time)
  df
}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' store_post_details
#'
#' @description Collect data frame of post details
#'
#' @note Some posts e.g. 'Events', do not come in desired format,
#' so there's a need to condition on that possibility
#' @param conn - an SQLite database connection
#' @param data - a dataframe returned by \code{Rfacebook::getPage}
#'
#' @importFrom Rfacebook getPost
#' @importFrom RSQLite dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
store_post_details <- function(conn, data)
{
  ## Pick an ID and use it to download details related to a particular post
  ## This function takes a while, so it's good to keep user abreast on
  ## progress
  ## Set up variables as much as possible to reduce indexing computations
  cat("Obtaining details for individual posts\n")
  len <- length(data$id)
  PB <- txtProgressBar(max = len, style = 3, char = "-")
  nesreaToken <- fetch_token()
  nesreaToken <- nesreaToken$token

  for (i in 1:len) {
    ID <- data$id[i]
    post_details <- getPost(post = ID, n = 1000, token = nesreaToken)

    ## Names of tables of interest in the local database
    local.tables <-
      c("nesreanigeria_fblikes", "nesreanigeria_fbcomments")
    sapply(local.tables, function(tb_name) {

      ## Extract the string 'like' or 'comments' from the table names
      abbr <-
        substr(tb_name, regexpr("fb", tb_name, ignore.case = TRUE) + 2,
               nchar(tb_name))


      if (abbr %in% names(post_details)) {
        ## Collect data frame; add a column for post ID
        detail_df <- post_details[[abbr]] %>%
          mutate(post_id = ID)

        ## Append the new data to the existing local table
        ## or create a brand new table if first-time use
        dbWriteTable(conn, tb_name, detail_df, append = TRUE)
      }
    })

    setTxtProgressBar(PB, i)   # increment the progress bar
  }
  cat("\n")
}
