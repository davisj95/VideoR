#' YouTube Data API Channel Wrapper
#'
#' This function returns Title, Description, CustomURL (if applicable),
#' Published Date/Time, Country, and number of Views, Comments, Subscribers,
#' and Videos.
#'
#' @param channelId character string
#' @param token environment
#'
#' @return df data.frame
#'
#' @examples
#' ytdata_channel_stats("as09df8yaidfjkl", token_var)
#'
#' @export
#'

ytdata_channel_stats <- function(channelId = NULL, token = NULL) {
  if(is.null(channelId) | is.null(token)){

    stop("A channel id AND token are required to get stats")

  } else {

    channelId <- trimws(channelId)
    df <- data.frame()

    for(i in 1:length(channelId)){

      url <- paste0("https://www.googleapis.com/youtube/v3/channels?part=snippet,statistics&id=",channelId[i])
      temp <- get_req(url, token)$items$snippet %>%
        select(-thumbnails, -localized) %>%
        bind_cols(temp$items$statistics)
      df <- bind_rows(df, temp)

    }
  }
  return(df)
}
