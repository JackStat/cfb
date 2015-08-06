#' @title trim Trim leading and trailing spaces
#' 
#' 
#' 
#' 
#' @export



trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}
