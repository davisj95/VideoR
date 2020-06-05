#' YouTube Analytics API Wrapper
#'
#' This function returns number of Shares by sharing service
#'
#' @param token environment
#' @param maxResults numeric
#' @param startDate character string
#' @param endDate character string
#'
#' @return df data.frame
#'
#' @examples
#' ytanalytics_sharing_service(token_var, 10, "2020-05-01", "2020-05-31")
#'
#' @export
#'

ytanalytics_sharing_service <- function(token = NULL, maxResults = 10,
                                        startDate = Sys.Date() - 30, endDate = Sys.Date()){

  if(is.null(token)) stop("A token is required to get stats")

  dim <- "sharingService"
  met <- "shares"
  sor <- "-shares"

  df <- ytanalytics_request(dim, met, sort, maxResults = maxResults,
                            startDate = startDate, endDate = endDate, token = token)
  return(df)
}
