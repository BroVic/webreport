globalVariables("nesreaToken")
#' Use the NESREA Facebook Page ID
#'
#' @description Use NESREA Page Id
#'
#' @return Returns the NESREA Facebook page
use_pg_id <- function()
{
  "145175075891647"
}
#'
#'
#'
#'
#'
#'
#'
#' get_api_version
#'
#' @details Returns the Facbook API version in use
get_api_version <- function()
{
  "2.8"
}
#'
#'
#'
#'
#'
#' fetch_token
#
#' @details Checks for local storage of the access token. If it is already present
#' we also check whether it has expired or not, in line with Facebook's
#' policy on token changes. The App credentials used (App Id & App Secret)
#' are as available via the App dashboard,
#'
#' @importFrom utils winDialog
#' @importFrom utils menu
fetch_token <- function()
{
  tkFile <- system.file("keys", "NESREA_fboauth", "webreport")
  app_id <- "203440573439361"
  app_secret <- "9957dccac2ebcef3fd0c79128edd47bd"
  if (nesreaToken$expiryDate <= Sys.Date()) {
    val <- NULL
    if (interactive()) {
      msg <- "This action will renew Facebook OAuth credentials. Continue?"
      if (identical(.Platform$OS.type, "windows"))
        val <- winDialog(type = "yesno", message = msg)
      else val <- menu(choices = c("Yes", "No"), title = msg)
    }
    else {
      message("The Facebook token has expired or is non-existent.")
      message("Open R to fix this (Administrator priviledges required.")
    }
    if (identical(val, "YES") | identical(val, 1L)) {
      save(nesreaToken,
           file = system.file("keys", "NESREA_fboauth", package = "webreport"))
    }
    else return(NULL)
  }
  invisible(nesreaToken)
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
#' fbTokenObj
#'
#' @description Constructor an S3 object containing an access token and its
#' expiry date
#'
#' @param app_id The Facebook App ID
#' @param app_secret The Facebook App Secret
#
#' @details The key thing about this function is that we want to be able to
#' keep in store the expected expiry date the token according to the
#' prevailing Facebook API policy, so that on loading it we can
#' confirm whether it is still valid or not.
#'
#' @return An S3 Object containing a generated Facebook access token and the
#' token's expiration date.
#'
#' @importFrom Rfacebook fbOAuth
fbTokenObj <- function(app_id, app_secret)
{
  structure(
    list(token = fbOAuth(app_id, app_secret),
         expiryDate = Sys.Date() + 60),
    class = "fbTokenObj")
}



