#' YouTube Analytics API Wrapper
#'
#' This function returns number of Views, Likes, Dislikes, and Comments
#' on top videos (Can adjust date range and max results (from 1-200))
#'
#' @param token environment
#' @param maxResults numeric
#' @param startDate character string
#' @param endDate character string
#'
#' @return df data.frame
#'
#' @examples
#' ytanalytics_top_videos(token_var, 10, "2020-05-01", "2020-05-31")
#'
#' @export
#'

ytanalytics_top_videos <- function(token = NULL, maxResults = 10, channelId = "MINE",
                                   startDate = Sys.Date() - 30, endDate = Sys.Date()) {

  if(is.null(token)) stop("A token is required to get stats")

  dim <- "video"
  met <- "views,averageViewDuration,shares,likes,dislikes,comments,subscribersGained,subscribersLost"
  sor <- "-views"

  df <- ytanalytics_request(dim, met, sort, maxResults = maxResults, startDate = startDate,
                            endDate = endDate, token = token)

  temp <- ytdata_video_metadata(df$video, token)

  titles <- character()
  for(i in df$video){
    url <- paste0("https://www.googleapis.com/youtube/v3/videos?part=snippet&id=",i)
    rList <- get_req(url, token)
    titles <- c(titles,rList$items$snippet$title)
  }
  df$title <- titles
  return(df)
}
