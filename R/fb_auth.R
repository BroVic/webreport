## fb_auth.R
## Functions around the Facebook OAuth credentials.
globalVariables("nesreaToken")


use_pg_id <- function()
{
  "145175075891647"
}







get_api_version <- function()
{
  "2.8"
}









#' @importFrom utils menu
fetch_token <- function()
{
  nesreaToken <- find_token("keys/NESREA_fboauth")
  if (nesreaToken$expiryDate <= Sys.Date()) {
    val <- NULL
    if (interactive()) {
      val <- menu(choices = c("Yes", "No"),
                  title = "Renew Facebook access token?")
    }
    else {
      message("Your Facebook access token has expired or is non-existent.")
	  txt <- if (interactive())
	      'renew_fb_cred()'
        else
          'Rscript -e "webreport::renew_fb_cred()"'
      message("Run", txt, "to get a new one (Admin role required).")
      return(NULL)
    }
    if (identical(val, 1L)) {
      renew_fb_cred()
    }
    else
      return(NULL)
  }
  invisible(nesreaToken)
}







## Get location of previously stored token
find_token <- function(key.loc = "keys/NESREA_fboauth")
{
  tkFile <- system.file(key.loc, package = "webreport")
  if (nchar(tkFile) > 0)
    load(tkFile)
  else
    message("Access token was not found.")
  invisible(nesreaToken)
}








# The key thing about the next function is that we want to be able to
# keep in store the expected expiry date the token according to the
# prevailing Facebook API policy, so that on loading it we can
# confirm whether it is still valid or not.

# It returns an S3 Object containing a generated Facebook access token and the
# token's expiration date.

#' @importFrom Rfacebook fbOAuth
#' @importFrom utils browseURL
fbTokenObj <- function(app_id, app_secret)
{
  browseURL('https://developers.facebook.com/apps/203440573439361/settings/basic/')
  structure(
    list(token = fbOAuth(app_id, app_secret),
         expiryDate = Sys.Date() + 60),
    class = "fbTokenObj")
}



#' Check Access Token Validity
#'
#' Check the expiry date of a Facebook access token.
#'
#' @details The S3 object \code{fbTokenObj} has a member that represents the
#' date the expected expiry date as determined by the exisiting Facebook
#' policy for access tokens.
#'
#' @export
token_expiry <- function()
{
  nesreaToken <- find_token()
  nesreaToken$expiryDate
}





#' Renew Facebook Token
#'
#' Obtain a fresh Facebook access token upon expiry of the extant one
#'
#' @details When run, the Facebook authentication process is initiated, which
#' involves copying and pasting a URL into the relevant field in the App's
#' 'Settings' page. For more information on Facebook authentication see
#' \code{\link[Rfacebook]{fbOAuth}}.
#'
#' @note The two credentials that are used to obtain the token (i.e. the App ID
#' and the App secret) are specific to NESREA. Only users with access to the
#' App's 'Settings' page at \url{https://developers.facebook.com/} can effect
#' the renewal.
#'
#' @export
renew_fb_cred <- function() {
  if (token_expiry() > Sys.Date())
    stop("Token has not yet expired.")
  nesreaToken <-
    fbTokenObj(203440573439361, "9957dccac2ebcef3fd0c79128edd47bd")
  Sys.sleep(2)
  save(nesreaToken,
       file = system.file("keys/NESREA_fboauth", package = "webreport"))
}
