#' Helper Function to send API requests
#'
#' Takes in custom url and token to get request
#'
#' @param url character
#' @param token environment
#'
#' @return temp data.frame
#'
#' @examples
#' get_req("https://example.com", token_var)
#'
#' @export
#'

get_req <- function(url, token){
  r <- httr::GET(url, token)
  httr::stop_for_status(r)
  temp <- jsonlite::fromJSON(paste0(rawToChar(r$content)))
  return(temp)
}
