#' YouTube Analytics API Wrapper
#'
#' This function returns number of Views, Shares, Likes, Dislikes,
#' Comments, Subs Gained & Lost for the channel (Can adjust date range)
#'
#' @param token environment
#' @param startDate character string
#' @param endDate character string
#'
#' @return df data.frame
#'
#' @examples
#' ytanalytics_channel_stats(token_var, "2020-05-01", "2020-05-31")
#'
#' @export
#'

ytanalytics_channel_stats <- function(token = NULL, startDate = Sys.Date() - 30, endDate = Sys.Date()){

  if(is.null(token)) stop("A token is required to get stats")

  met <- "views,shares,likes,dislikes,comments,subscribersGained,subscribersLost"
  sort <- "-views"

  df <- ytanalytics_request(NULL, met, sort, maxResults = maxResults,
                            startDate = startDate, endDate = endDate, token = token)
  return(df)
}
