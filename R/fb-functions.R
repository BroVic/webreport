globalVariables(c("value", "end_time"))
# Collect data on Insights from a Facebook Page.
#
# Retrieves and preprocesses insights from NESREA Facebook Page
#
# @param type The type of Facebook insight being queried.
# @note This is a wrapper for \code{Rfacebook::getInsights}
#
# @importFrom dplyr select
# @importFrom Rfacebook getInsights
## Export when fixed
## TODO: Remove references to dplyr
# choose_insight <-
#   function(type = c(
#     "page_fan_adds",
#     "page_fan_removes",
#     "page_views_login",
#     "page_views_logout",
#     "page_views",
#     "page_story_adds",
#     "page_impressions",
#     "page_posts_impressions",
#     "page_consumptions",
#     "post_consumptions_by_type",
#     "page_fans_country"
#   ))
#   {
#     NESREA_page_id <- use_pg_id()
#     API_version <- get_api_version()
#     nesreaToken <- fetch_token()
#     nesreaToken <- nesreaToken$token
#     result <-
#       getInsights(
#         object_id = NESREA_page_id,
#         token = nesreaToken,
#         metric = type,
#         version = API_version
#       )
#     result <- select(result, value:end_time)
#     result$end_time <- substr(result$end_time,
#                               start = 1,
#                               stop = regexpr("T", result$end_time) - 1)
#     result
#   }









## Prepare Downloaded Facebook Data for further Processing.
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













#' @importFrom Rfacebook getPost
#' @importFrom RSQLite dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
store_post_details <- function(keyword, conn, data)
{
  ## Pick an ID and use it to download details related to a particular post
  ## This function takes a while, so it's good to keep users abreast on
  ## progress.
  ## Set up variables as much as possible to reduce indexing computations
  cat("Obtaining 'comments' and 'likes' for individual posts\n")
  len <- length(data$id)
  numPosts <- 5000
  PBar <- txtProgressBar(max = len, style = 3, char = "-")
  nesreaToken <- fetch_token()
  nesreaToken <- nesreaToken$token

  for (i in 1:len) {
    ID <- data$id[i]
    post_details <- getPost(post = ID, n = numPosts, token = nesreaToken)
    if (identical(length(post_details), numPosts))
      warning(sprintf(
        "You downloaded the maximum of %s posts.", numPosts))
    local.tables <-
      sapply(c("_fblikes", "_fbcomments"), function(x) paste0(keyword, x))
    sapply(local.tables, function(tb_name) {
      abbr <-
        substr(tb_name, regexpr("fb", tb_name, ignore.case = TRUE) + 2,
               nchar(tb_name))

      ## Collect data frame; add a column for post ID
      if (abbr %in% names(post_details)) {
        detail_df <- post_details[[abbr]] %>%
          mutate(post_id = ID)

        ## Append the new data to the existing local table
        ## or create a brand new table if first-time use
        dbWriteTable(conn, tb_name, detail_df, append = TRUE)
      }
    })
    setTxtProgressBar(PBar, i)
  }
  cat("\n")
}
