#' YouTube Data API Channel Wrapper
#'
#' This function pulls a channel's most recent video(s)
#'
#' @param channelId character string
#' @param token environment
#' @param maxResults numeric
#'
#' @return df data.frame
#'
#' @examples
#' ytdata_channel_most_recent_video("as09df8yaidfjkl", token_var)
#'
#' @export
#'

ytdata_channel_most_recent_video <- function(channelId = NULL, token = NULL, maxResults=1){
  if(is.null(channelId) | is.null(token)){

    stop("A channel id AND token are required to get stats")

  } else {

    channelId <- trimws(channelId)
    df <- data.frame()

    for(i in 1:length(channelId)){
      url <- paste0("https://www.googleapis.com/youtube/v3/search?part=snippet&channelId=",
                    channelId[i],"&maxResults=",maxResults,"&order=date&type=video")
      temp <- get_req(url, token)
      if(!is.null(temp$items)){
        temp <- temp %>%
          select(publishedAt, channelId)
        df <- bind_rows(df,temp)
      }
    }
  }
  return(df)
}
