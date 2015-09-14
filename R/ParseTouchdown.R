#' @title Identify touchdowns
#' 
#' 
#' @export


ParseTouchdown <- function(x){
  x$Touchdown = grepl('touchdown', x[,'scoreText'])
  x
}

