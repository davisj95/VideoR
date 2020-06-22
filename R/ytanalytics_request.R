#' YouTube Analytics API Wrapper
#'
#' This function is an API helper function for pulling
#' YT Analytics data on your own channel
#'
#' @param dimensions character string
#' @param metrics character string
#' @param sort character string
#' @param maxResults numeric
#' @param filtr character string
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

ytanalytics_request <- function(dimensions = NULL, metrics = NULL, sort = NULL,
                                maxResults = NULL, filtr = NULL, startDate = Sys.Date() - 30,
                                endDate = Sys.Date(), token) {

  url <- paste0("https://youtubeanalytics.googleapis.com/v2/reports?&ids=channel%3D%3DMINE",
                "&startDate=", startDate, "&endDate=", endDate)

  if(!is.null(dimensions)) url <- paste0(url, "&dimensions=", dimensions)
  if(!is.null(metrics)) url <- paste0(url, "&metrics=", metrics)
  if(!is.null(sort)) url <- paste0(url, "&sort=", sort)
  if(!is.null(maxResults)) url <- paste0(url, "&maxResults=", maxResults)
  if(!is.null(filtr)) url <- paste0(url, "&filters=", filtr)

  r <- get_req(url, token)
  if(length(r$rows) > 0){
    df <- as.data.frame(r$rows)
    colnames(df) <- r$columnHeaders$name
    return(df)
  }
}
