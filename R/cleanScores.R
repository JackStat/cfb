#' @title cleans Scores from NCAA scrape
#' @export

cleanScores <- function(x){
  
  Scores <- suppressWarnings(as.numeric(as.character(x)))
  cummax(ifelse(is.na(Scores), 0, Scores))
  
}
