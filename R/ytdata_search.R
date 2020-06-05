#' YouTube Data API Search Wrapper
#'
#' This function returns Publish Date/Time, Channel ID, Title, Description,
#' Channel Title, and LiveBroadcastContent (if Applicable) for search term
#'
#' @param term character string
#' @param token environment
#' @param maxResults numeric
#'
#' @return df data.frame
#'
#' @examples
#' ytdata_search("Sail Cat", token_var)
#'
#' @export
#'

ytdata_search <- function(term = NULL, token = NULL, maxResults = 10){
  if(is.null(term) | is.null(token)){

    stop("A term AND token are required to get stats")

  } else {

    term <- term %>%
      trimws() %>%
      tolower() %>%
      gsub(" ", "+", .)

    url <- paste0("https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=",
                  maxResults,"&q=",term)
    temp <- get_req(url, token)$items$snippet %>%
      select(-thumbnails)

  }
  return(temp)
}
