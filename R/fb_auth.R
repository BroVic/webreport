## fb_auth.R
## Functions around the Facebook OAuth credentials.
globalVariables("myToken")


FbCredentials <-
  setRefClass(
    'FbCredentials',
    fields = list(
      appId = 'character',
      appSecret = 'character',
      apiVersion = 'character',
      pageId = 'character'
    ),
    methods = list(
      set_appId = function() {
        appId <<- '203440573439361'
      },
      set_appSecret = function() {
        appSecret <<- "9957dccac2ebcef3fd0c79128edd47bd"
      },
      set_apiVersion = function() {
        apiVersion <<- '2.8'
      },
      set_pageId = function() {
        pageId <<- "145175075891647"
      },
      get_appId = function() {
        as.character(appId)
      },
      get_appSecret = function() {
        as.character(appSecret)
      },
      get_apiVersion = function(){
        as.character(apiVersion)
      },
      get_pageId = function() {
        as.character(pageId)
      }
    )
  )

## Create an instance of the FbCredentials class
cred <- new('FbCredentials')




#' @importFrom utils menu
fetch_token <- function()
{
  myToken <- findToken("keys/my_fboauth")
  if (myToken$expiryDate <= Sys.Date()) {
    fn <- quote(renew_fb_cred())
    val <- NULL
    if (interactive()) {
      val <- menu(choices = c("Yes", "No"),
                  title = "Renew Facebook access token?")
    }
    else {
      fnStr <- deparse(fn)
      txt <- if (interactive()) {
        fnStr
      }
      else {
        sprintf('Rscript -e "webreport::%s"\n', fnStr)
      }
      message("Your Facebook access token has expired or is non-existent.")
      message("Run ", txt, " to get a new one (Admins only)")
      return(NULL)
    }

    if (identical(val, 1L)) {
      eval(fn)
    }
    else
      return(NULL)
  }
  invisible(myToken)
}







## Get location of previously stored token
findToken <- function(key.loc = "keys/my_fboauth")
{
  tkFile <- system.file(key.loc, package = "webreport")
  if (nchar(tkFile) > 0)
    load(tkFile)
  else
    message("Access token was not found.")
  invisible(myToken)
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
  url <- 'https://developers.facebook.com/apps/203440573439361/settings/basic/'
  browseURL(url)
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
#' @examples
#' inherits(token_expiry(), 'Date')
#'
#' @export
token_expiry <- function()
{
  myToken <- findToken()
  myToken$expiryDate
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
  myToken <-
    fbTokenObj(cred$get_appId(), cred$get_appSecret())
  Sys.sleep(2)
  save(myToken,
       file = system.file("keys/my_fboauth", package = "webreport"))
}
