#' YouTube Data API Channel Wrapper
#'
#' This function loops through each video in each playlist to get all
#' channel videos and their stats
#'
#' @param channelId character string
#' @param token environment
#'
#' @return df data.frame
#'
#' @examples
#' ytdata_channel_videos("as09df8yaidfjkl", token_var)
#'
#' @export
#'

ytdata_channel_videos <- function(channelId = NULL, token = NULL){
  if(is.null(channelId) | is.null(token)){

    stop("A channel id AND token are required to get stats")

  } else {
    playlistIds <- ytdata_channel_playlists(channelId, token)$playlistId
    df <- ytdata_playlist_videos(playlistIds, token)
    return(df)
  }
}
