#' YouTube API OAuth
#'
#' This creates/grabs token to authorize data pulls
#'
#' @param appId character string
#' @param appSecret character string
#' @param token environment
#'
#' @return token environment
#'
#' @examples
#' youtube_oauth("as09df8yaidfjkl", "0a9843jtoaiewfn", ".httr-oauth")
#'
#' @export
#'

youtube_oauth <- function (appId = NULL, appSecret = NULL, token = NULL) {

  if(is.null(appId) | is.null(appSecret)){

    stop("Missing appId or appSecret")

  }  else if(file.exists(token)) {

    google_token <- try(suppressWarnings(readRDS(token)), silent = TRUE)

    if ( inherits(google_token, "try-error")) {
      stop(sprintf("Unable to read token from:%s", token))
    }

    google_token <- google_token[[1]]
  } else {

    google_token <- httr::oauth2.0_token(httr::oauth_endpoints("google"),
                                         httr::oauth_app("google", appId, appSecret),
                                         scope = c("https://www.googleapis.com/auth/youtube.readonly",
                                                   "https://www.googleapis.com/auth/yt-analytics.readonly",
                                                   "https://www.googleapis.com/auth/youtubepartner"))

  }

  return(google_token)
}
