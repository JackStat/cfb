#' @title Scrape NCAA json files.
#' 
#' 
#' @import dplyr
#' 
#' @export

NCAApbp <- function(url){
    raw<-suppressWarnings(readLines(url))

    scoreText <- raw[grepl('"scoreText"', raw)]
    scoreTextW <- which(grepl('"scoreText"', raw))
    
    driveText <- raw[grepl('"driveText"', raw)]
    visitingScore <- raw[grepl('"visitingScore"', raw)]
    homeScore <- raw[grepl('"homeScore"', raw)]
    
    teamId <- raw[grepl('"teamId"', raw)]
    teamIdW <- cumsum(grepl('"teamId"', raw))
    
    time <- raw[grepl('"time"', raw)]
    timeW <- cumsum(grepl('"time"', raw))
    
    shortTitle <- raw[grepl('"shortTitle"', raw)]
    shortTitleW <- cumsum(grepl('"shortTitle"', raw))
    
    
    data.frame(
        scoreText = cleanText(scoreText)
        ,driveText = cleanText(driveText)
        ,visitingScore = cleanText(visitingScore)
        ,homeScore = cleanText(homeScore)
        ,GameTime = cleanText(time[timeW[scoreTextW]])
        ,TeamID = cleanText(teamId[teamIdW[scoreTextW]])
        ,Quarter = cleanText(shortTitle[shortTitleW[scoreTextW]])
        ,stringsAsFactors = FALSE
        ) %>%
        mutate(
            visitingScore = cleanScores(visitingScore)
            ,homeScore = cleanScores(homeScore)
            )
}



