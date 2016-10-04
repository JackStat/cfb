espnOpp<-function(x){
  
  # -Remove vs and @
  iter1<-gsub("vs|@", "", x)
  # -Remove ranking from name
  iter2<-gg2<-gsub("[#0-9]", "", iter1)
  # -Trim whitspace
  iter3<-gsub("^\\s+|\\s+$", "", iter2)
  # -Remove asterisks
  iter4<-gsub("\\*", "", iter3)
  return(iter4)
  
}



espnDate<-function(x){
    # - Removing weekday
    noWday<-substr(x,6,20)
    # -fixing September
    fixSept<-gsub("Sept", "Sep", noWday)
    # -converting to date
    as.Date(fixSept, "%b %d %Y")
}