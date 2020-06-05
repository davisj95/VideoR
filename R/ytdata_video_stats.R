#' YouTube Data API Video Wrapper
#'
#' Takes in vector of ids and a token to get overall number of Views,
#' Likes, Dislikes, Favorites, and Comments of each video
#'
#' @param ids character vector
#' @param token environment
#'
#' @return df data.frame
#'
#' @examples
#' ytdata_video_stats("21ljkho84", token_var)
#'
#' @export
#'

ytdata_video_stats <- function(ids = NULL, token = NULL) {
  if(is.null(ids) | is.null(token)){

    stop("A video id AND token are required to get stats")

  } else {

    df <- data.frame()
    ids <- clean_id(ids)

    for(i in 1:length(ids)){
      print(i)
      url <- paste0("https://www.googleapis.com/youtube/v3/videos?part=statistics&id=",ids[i])
      temp <- get_req(url, token)
      if(length(temp$items)!= 0){
        temp <- temp$items$statistics %>%
          mutate(video = ids[i])
        df <- bind_rows(df, temp)
      }
    }
  }
  return(df)
}
