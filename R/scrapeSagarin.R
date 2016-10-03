#' @title Pull in Sagarin's Ratings
#' 
#' 
#' @import XML RCurl stringr
#' 
#' 
#' @export


scrapeSagarin <- function(){
  
  theurl <- "http://www.usatoday.com/sports/ncaaf/sagarin/"
  
  doc = htmlParse(theurl)
  
  bs <- getNodeSet(doc, "//b")
  
  Bs <- sapply(bs, FUN = xmlValue)
  timePeriodRated = Bs[grepl('COLLEGE FOOTBALL', Bs)][1]
  
  tableNodes1 <- getNodeSet(doc, '//*[(@id = "sagarin")]//font//font')
  
  temp <- sapply(X = tableNodes1, FUN = xmlValue)
  
  teamsW<-which((str_detect(temp, "AA =") | 
                   str_detect(temp, "A  =")) & 
                  !str_detect(temp, "ELO"))
  
  
#   temp<-xpathSApply(doc, '//font', xmlValue)
#   
#   teamsW <- which((str_detect(temp, "AA =") | 
#                    str_detect(temp, "A  =")) & 
#                   !str_detect(temp, "ELO"))
#   
  
  temp<-gsub("&nbsp", "", temp)
  temp<-gsub(" AA", "  AA", temp)
  temp<-gsub("\\(", " ", temp)
  temp<-gsub(")", " ", temp)
  temp<-gsub("\\|", " ", temp)
  temp<-gsub("\\=", " ", temp)
  temp<-trim(temp)
  
  temp0<-
    paste(
      temp[teamsW]
    , temp[teamsW+1]
    , temp[teamsW+2]
    , temp[teamsW+3]
    , temp[teamsW+4]
    , temp[teamsW+5]
    , sep = "  ")
  
  temp0<-gsub(" {2,}", ",", temp0)
  
  # temp0<-temp0[-1]
  
  sag <- do.call(rbind, strsplit(temp0, ","))
  
  Sag<-data.frame(sag, stringsAsFactors=FALSE)
  # - remove single quotes in team names
  Sag$X2 <- gsub("'", '', Sag$X2)
  
  colnames(Sag)<-c(
     "rank"
    ,"team"
    ,"division"
    ,"rating"
    ,"wins"
    ,"losses"
    # ,"ties"
    ,"schedule_rating"
    ,"schedule_rank"
    ,"win_top_10"
    ,"loss_top_10"
    # ,"ties_top_10"
    ,"win_top_30"
    ,"loss_top_30"
    # ,"ties_top_30"
    ,"predictor_rating"
    ,"predictor_rank"
    ,"golden_mean_rating"
    ,"golden_mean_rank"
    ,"recent_rating"
    ,"recent_rank"
    )
  
  list(
    request_ts = Sys.time()
    ,timePeriodRated = timePeriodRated
    ,Rankings = Numerics(Sag)  
  )
  
}



