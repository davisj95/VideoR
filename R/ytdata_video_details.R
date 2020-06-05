#' YouTube Data API Video Wrapper
#'
#' This function returns PlayTime, Dimension(?), Definition (HD, etc),
#' Captions (bool), and License (bool)
#'
#' @param ids character vector
#' @param token environment
#'
#' @return df data.frame
#'
#' @examples
#' ytdata_video_details("21ljkho84", token_var)
#'
#' @export
#'

ytdata_video_details <- function(ids = NULL, token = NULL) {
  if(is.null(ids) | is.null(token)){

    stop("A video id AND token are required to get stats")

  } else {

    df <- data.frame()
    ids <- clean_id(ids)

    for(i in 1:length(ids)){

      url <- paste0("https://www.googleapis.com/youtube/v3/videos?part=contentDetails&id=",ids[i])
      temp <- get_req(url, token)
      if(length(temp$items) != 0){
        temp <- temp$items$contentDetails %>%
          select(-contentRating) %>%
          mutate(video = ids[i])
        df <- bind_rows(df, temp)
      }
    }
  }
  return(df)
}
