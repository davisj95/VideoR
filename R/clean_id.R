#' Helper Function to clean ids fed to other functions
#'
#' Takes in vector of ids to remove NAs and whitespace.
#'
#' @param ids character vector
#'
#' @return ids character vector
#'
#' @examples
#' clean_id(c(" 12hi5nh ", "12rfe34rt", NA))
#'
#' @export
#'

clean_id <- function(ids){
  ids <- ids %>%
    na.omit(.) %>%
    trimws(.)
  return(ids)
}
