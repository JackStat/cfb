#' @title cleans Scores from NCAA scrape
#' @export

cleanScores <- function(x){
    cummax(ifelse(x == '', 0, as.numeric(as.character(x))))
}
