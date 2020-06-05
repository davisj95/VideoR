#' YouTube Data API Video Wrapper
#'
#' This function returns Publish Date/Time, Channel ID, Title, Description, Tags,
#' Category ID, LiveBroadcastContent (if applicable), and DefaultAudioLanguage
#'
#' @param ids character vector
#' @param token environment
#'
#' @return df data.frame
#'
#' @examples
#' ytdata_video_metadata("21ljkho84", token_var)
#'
#' @export
#'

ytdata_video_metadata <- function(ids = NULL, token = NULL) {
  if(is.null(ids) | is.null(token)){

    stop("A video id AND token are required to get meta data")

  } else {

    df <- data.frame()
    ids <- clean_id(ids)

    for(i in 1:length(ids)){

      url <- paste0("https://www.googleapis.com/youtube/v3/videos?part=snippet&id=",ids[i])
      temp <- get_req(url,token)

      if(length(temp$items) != 0) {
        temp <- temp$items$snippet %>%
          select(-thumbnails, -localized) %>%
          mutate(video = ids[i])
        df <- bind_rows(df, fin)
      }
    }
  }
  return(df)
}
