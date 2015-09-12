#' @title Scrape NCAA team stats
#' 
#' @import jsonlite
#' @export




NCAAteamStats<- function(url){
    
  raw<-suppressWarnings(readLines(url))
  
  rawfromJSON <- fromJSON((gsub('callbackWrapper\\(','',gsub('\\}\\)\\;','\\}',raw))))
  
  tStats <- foreach(k = 1:length(rawfromJSON$teams), .combine = 'rbind') %do% {
    tt <- foreach(i = 1:length(rawfromJSON$teams$stats[[1]]$stat), .combine = 'rbind') %do% {
    
    if(is.null(rawfromJSON$teams$stats[[k]]$breakdown[[i]])){
      tStats = data.frame(
        teamId = rawfromJSON$teams$teamId[k]
        ,stat = NA
        ,data = NA
        ,Stat = rawfromJSON$teams$stats[[k]]$stat[i]
        ,Data = rawfromJSON$teams$stats[[k]]$data[i]
      )
    } else {
      
      tStats = data.frame(
        teamId = rawfromJSON$teams$teamId[k]
        ,rawfromJSON$teams$stats[[k]]$breakdown[[i]]
        ,Stat = rawfromJSON$teams$stats[[k]]$stat[i]
        ,Data = rawfromJSON$teams$stats[[k]]$data[i]
      )
      
    }
    }
    
    tt %>%
      group_by(teamId) %>%
      summarise(
        FirstDowns = Data[Stat == '1st Downs'][1]
        ,Rushing1stDowns = data[Stat == '1st Downs' & stat == 'Rushing']
        ,Passing1stDowns = data[Stat == '1st Downs' & stat == 'Passing']
        ,Penalty1stDowns = data[Stat == '1st Downs' & stat == 'Penalty']
        ,RushingYards = Data[Stat == 'Rushing'][1]
        ,RushingAttempts = data[Stat == 'Rushing' & stat == 'Attempts']
        ,PassingYards = Data[Stat == 'Passing'][1]
        ,PassingAttempts = data[Stat == 'Passing' & stat == 'Attempts']
        ,Completions = data[Stat == 'Passing' & stat == 'Completions']
        ,PassingInterceptions = data[Stat == 'Passing' & stat == 'Interceptions']
        ,OffensivePlays = Data[Stat == 'Total Offense'][1]
        ,Fumbles = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\1', Data[Stat == 'Fumbles: Number-Lost'][1])
        ,FumblesLost = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\2', Data[Stat == 'Fumbles: Number-Lost'][1])
        ,Penalties =  gsub('([0-9]{,3})\\-([0-9]{,3})', '\\1', Data[Stat == 'Penalties: Number-Yards'][1])
        ,PenaltyYards =  gsub('([0-9]{,3})\\-([0-9]{,3})', '\\2', Data[Stat == 'Penalties: Number-Yards'][1])
        ,Punts = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\1', Data[Stat == 'Punting: Number-Yards'][1])
        ,PuntYards = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\2', Data[Stat == 'Punting: Number-Yards'][1])
        ,PuntReturns = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\1', Data[Stat == 'Punt Returns: Number-Yards'][1])
        ,PuntReturnYards = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\2', Data[Stat == 'Punt Returns: Number-Yards'][1])
        ,KickoffReturns = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\1', Data[Stat == 'Kickoff Returns: Number-Yards'][1])
        ,KickoffReturnYards = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\2', Data[Stat == 'Kickoff Returns: Number-Yards'][1])
        ,Interceptions = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\1', Data[Stat == 'Interception Returns: Number-Yards'][1])
        ,Interceptions = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\2', Data[Stat == 'Interception Returns: Number-Yards'][1])
        ,ThirdDowns = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\2', Data[Stat == 'Third-Down Conversions'][1])
        ,ThirdDownConversions = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\1', Data[Stat == 'Third-Down Conversions'][1])
        ,FourthDownAttempts = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\2', Data[Stat == 'Fourth-Down Conversions'][1])
        ,FourthDownConversions = gsub('([0-9]{,3})\\-([0-9]{,3})', '\\1', Data[Stat == 'Fourth-Down Conversions'][1])
      ) %>% data.frame(stringsAsFactors = FALSE)
      
    
  }
  
  tStats %>%
    mutate(
      teamId = as.character(teamId)
    ) %>%
    left_join(rawfromJSON$meta$teams, by = c('teamId' = 'id'))
  
}