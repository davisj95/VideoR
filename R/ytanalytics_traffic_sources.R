#' YouTube Analytics API Wrapper
#'
#' This function returns the views by traffic type
#'
#' @param token environment
#' @param maxResults numeric
#' @param startDate character string
#' @param endDate character string
#'
#' @return df data.frame
#'
#' @examples
#' ytanalytics_traffic_sources(token_var, 10, "2020-05-01", "2020-05-31")
#'
#' @export
#'

ytanalytics_traffic_sources <- function(token, maxResults = 10,
                                        startDate = Sys.Date()-30, endDate = Sys.Date()) {

  if(is.null(token)) stop("A token is required to get stats")

  dim <- "insightTrafficSourceType"
  met <- "views"
  sor <- "-views"

  df <- ytanalytics_request(dim, met, sor, maxResults = maxResults,
                            startDate = startDate, endDate = endDate, token = token)
  return(df)
}
