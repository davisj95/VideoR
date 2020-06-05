#' YouTube Data API Channel Wrapper
#'
#' This function returns ALL playlists in a channel and their Published Date/Time,
#' Title, Description, Channel Title & ID, Video Count and Playlist ID
#'
#' @param channelId character string
#' @param token environment
#'
#' @return df data.frame
#'
#' @examples
#' ytdata_channel_playlists("as09df8yaidfjkl", token_var)
#'
#' @export
#'

ytdata_channel_playlists <- function(channelId = NULL, token = NULL) {
  if(is.null(channelId) | is.null(token)){

    stop("A channel id AND token are required to get stats")

  } else {

    if(channelId == "mine"){
      baseURL <- paste0("https://www.googleapis.com/youtube/v3/playlists?",
                        "part=contentDetails%2Csnippet&maxResults=50&mine=true")
    } else {
      baseURL <- paste0("https://www.googleapis.com/youtube/v3/playlists?",
                        "part=contentDetails%2Csnippet&maxResults=50&channelId=",channelId)
    }

    pageToken <- "1stIt"
    allData <- data.frame()

    while(!is.null(pageToken)){
      if(pageToken == "1stIt"){
        url <- baseURL
      } else {
        url <- paste0(baseURL,"&pageToken=",pageToken)
      }

      temp <- get_req(url,token)
      df <- temp$items$snippet %>%
        select(-thumbnails, -localized) %>%
        mutate(videoCount = temp$items$contentDetails$itemCount,
               playlistId = temp$items$id)
      allData <- bind_rows(allData, df)
      pageToken <- temp$nextPageToken
    }
  }
  return(allData)
}
