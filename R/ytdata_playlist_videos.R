#' YouTube Data API Playlist Wrapper
#'
#' This function returns list of videos within a playlist
#'
#' @param playlistId character string
#' @param token environment
#'
#' @return df data.frame
#'
#' @examples
#' ytdata_playlist_videos("as09df8yaidfjkl", token_var)
#'
#' @export
#'

ytdata_playlist_videos <- function(playlistId = NULL, token = NULL) {
  if(is.null(playlistId) | is.null(token)){

    stop("A playlist id AND token are required to get stats")

  } else {

    allData <- data.frame()

    for(i in 1:length(playlistId)){

      baseURL <- paste0("https://www.googleapis.com/youtube/v3/playlistItems",
                        "?part=snippet&maxResults=50&playlistId=",playlistId[i])
      pageToken <- "1stIt"

      while(!is.null(pageToken)){

        if(pageToken == "1stIt"){
          url <- baseURL
        } else {
          url <- paste0(baseURL,"&pageToken=",pageToken)
        }
        temp <- get_req(url,token)

        if(length(temp$items) != 0){

          df <- temp$items$snippet %>%
            mutate(videoId = temp$items$snippet$resourceId$videoId) %>%
            select(-thumbnails, -resourceId)

          allData <- bind_rows(allData, df)
        }

        pageToken <- temp$nextPageToken
      }
    }
  }
  return(allData)
}
