#' @title Identify touchdowns
#' 
#' 
#' @export


ParseTouchdown <- function(x){
  grepl('touchdown', x[,'scoreText'])
}

