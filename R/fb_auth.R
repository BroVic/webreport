globalVariables("nesreaToken")






use_pg_id <- function()
{
  "145175075891647"
}







get_api_version <- function()
{
  "2.8"
}









#' @importFrom utils winDialog
#' @importFrom utils menu
fetch_token <- function()
{
  tkFile <- system.file("keys/NESREA_fboauth", package = "webreport")
  stopifnot(nchar(tkFile) > 0)
  load(tkFile)
  # TODO: What the heck was I doing with this?
  # app_id <- "203440573439361"
  # app_secret <- "9957dccac2ebcef3fd0c79128edd47bd"

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
           file = system.file("keys/NESREA_fboauth", package = "webreport"))
    }
    else return(NULL)
  }
  invisible(nesreaToken)
}








# The key thing about the next function is that we want to be able to
# keep in store the expected expiry date the token according to the
# prevailing Facebook API policy, so that on loading it we can
# confirm whether it is still valid or not.

# It returns an S3 Object containing a generated Facebook access token and the
# token's expiration date.

#' @importFrom Rfacebook fbOAuth
fbTokenObj <- function(app_id, app_secret)
{
  structure(
    list(token = fbOAuth(app_id, app_secret),
         expiryDate = Sys.Date() + 60),
    class = "fbTokenObj")
}



