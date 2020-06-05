#' YouTube Analytics API Wrapper
#'
#' This function returns number of Views, Shares, Likes, Dislikes, Comments,
#' Subs Gained & Lost for the channel by country (Can adjust date range)
#'
#' @param token environment
#' @param maxResults numeric
#' @param startDate character string
#' @param endDate character string
#'
#' @return df data.frame
#'
#' @examples
#' ytanalytics_country_stats(token_var, 10, "2020-05-01", "2020-05-31")
#'
#' @export
#'

ytanalytics_country_stats <- function(token = NULL, maxResults = 10,
                                      startDate = Sys.Date() - 30, endDate = Sys.Date()){

  if(is.null(token)) stop("A token is required to get stats")

  dim <- "country"
  met <- "views,shares,likes,dislikes,comments,subscribersGained,subscribersLost"
  sort <- "-views"

  df <- ytanalytics_request(dim, met, sort, maxResults = maxResults,
                            startDate = startDate, endDate = endDate, token = token)
  return(df)
}
