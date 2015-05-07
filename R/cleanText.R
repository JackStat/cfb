#' @title clean text from NCAA scrape
#' @export

cleanText <- function(x){
    t1 <- gsub('\t', '', x)
    t2 <- gsub(' {2,}', ' ', t1)
    t3 <- gsub('\"', '', t2)
    t4 <- gsub(',$', '', t3)
    t5 <- gsub(' scoreText : | driveText : | visitingScore : | homeScore : | time : | teamId : | shortTitle : ', '', t4)

    t5
    
}
