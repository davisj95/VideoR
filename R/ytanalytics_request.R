#' YouTube Analytics API Wrapper
#'
#' This function is an API helper function for pulling
#' YT Analytics data on your own channel
#'
#' @param dimensions character string
#' @param metrics character string
#' @param sort character string
#' @param maxResults numeric
#' @param filter character string
#' @param startDate character string
#' @param endDate character string
#' @param token environment
#'
#' @return df data.frame
#'
#' @examples
#' ytanalytics_request("video", "views", "-views", 10, NULL, "2020-05-01", 2020-05-31, token_var)
#'
#' @export
#'

ytanalytics_request <- function(dimensions = NULL, metrics, sort, maxResults = 10, filtr = NULL,
                                startDate = Sys.Date() - 30, endDate = Sys.Date(), token) {

  url <- paste0("https://youtubeanalytics.googleapis.com/v2/reports?",
                "&ids=channel%3D%3DMINE",
                "&metrics=", metrics,
                "&sort=", sort,
                "&maxResults=", maxResults,
                "&startDate=", startDate,
                "&endDate=", endDate)
  if(!is.null(filtr)) {
    url <- paste0(url, "&filter=", filtr)
  }
  if(!is.null(dimensions)) {
    url <-paste0(url,"&dimensions=", dimensions)
  }

  r <- get_req(url, token)
  df <- as.data.frame(r$rows)
  colnames(df) <- r$columnHeaders$name
  return(df)
}