#' @title ESPN calendar
#' 
#' @import XML foreach
#' 
#' @export


ESPNcalendar <- function() {
  allJSON <- fromJSON("http://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard")
  
  calRaw <- allJSON$leagues$calendar[[1]]$entries[[1]]
  
  calRaw$startDate <- as.Date(calRaw$startDate, "%Y-%m-%dT%H:%MZ")
  calRaw$endDate <- as.Date(calRaw$endDate, "%Y-%m-%dT%H:%MZ")
  
  foreach(i = 1:nrow(calRaw), .combine = 'rbind') %do% {
    
    Date = seq(calRaw$startDate[i], calRaw$endDate[i], by = 'day')
    
    data.frame(
      date = Date
      ,label = rep(calRaw$label[i], length(Date))
      ,value = rep(calRaw$value[i], length(Date))
    )
    
  }
  
}


