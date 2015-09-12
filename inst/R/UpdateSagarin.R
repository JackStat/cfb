library(RPostgres)
library(foreach)
library(DBI)
library(cfb)

con <- dbConnect(RPostgres::Postgres()
               , host='cfb.cjkdk120ktqn.us-west-1.rds.amazonaws.com'
               , port='5432'
               , dbname='cfb_master'
               , user='JackStat'
               , password='XG1372e7')

# - run Sagarin Scrape
ss <- try(scrapeSagarin())

# - pulling last entry in db
lastEntry <- 
  dbGetQuery(con
    ,'SELECT * FROM scrape
      ORDER BY scrape_ts desc
      LIMIT 1'
  )

if(trim(lastEntry$sagarin_title) == ss$timePeriodRated){
  
  
  ## - update scrape table to show no update
  updateScrape <- dbSendQuery(con
    ,paste0(
      'insert into scrape (scrape_ts, success, sagarin_updated, sagarin_title) values ('
      ,"'",ss$request_ts,"'", ','
      ,class(ss) !='try-error', ','
      ,'FALSE,'
      ,"'",ss$timePeriodRated,"');"
      )
    )
  # dbGetRowsAffected(updateScrape)

} else {
  
  # inserting scrape status
  updateScrape <- dbSendQuery(con
    ,paste0(
      ## - update scrape table for statuses
      'insert into scrape (scrape_ts, success, sagarin_updated, sagarin_title) values ('
      ,"'",ss$request_ts,"'", ','
      ,class(ss) !='try-error', ','
      ,'TRUE,'
      ,"'",ss$timePeriodRated,"');"
      )
    )
  
  getScrapeID <- 
    dbGetQuery(con
      ,paste0('SELECT * FROM scrape
        WHERE scrape_ts = ', "'", ss$request_ts, "'")
    )
  
  updateSagarin <- 
    function(i){
      dbSendQuery(con
      ,paste0(
        'insert into sagarin (scrape_id, rank, team, division, rating, wins, losses, ties, schedule_rating, schedule_rank, win_top_10, loss_top_10, ties_top_10, win_top_30, loss_top_30, ties_top_30, golden_mean_rating, golden_mean_rank, predictor_rating, predictor_rank',

# ', elo_rating, elo_rank
', recent_rating, recent_rank
        ) values ('
        ,getScrapeID$id, ','
        ,ss$Rankings$rank[i], ','
        ,"'", ss$Rankings$team[i],"'", ','
        ,"'", ss$Rankings$division[i],"'", ','
        ,ss$Rankings$rating[i], ','
        ,ss$Rankings$wins[i], ','
        ,ss$Rankings$losses[i], ','
        ,ss$Rankings$ties[i], ','
        ,ss$Rankings$schedule_rating[i], ','
        ,ss$Rankings$schedule_rank[i], ','
        ,ss$Rankings$win_top_10[i], ','
        ,ss$Rankings$loss_top_10[i], ','
        ,ss$Rankings$ties_top_10[i], ','
        ,ss$Rankings$win_top_30[i], ','
        ,ss$Rankings$loss_top_30[i], ','
        ,ss$Rankings$ties_top_30[i], ','
        ,ss$Rankings$golden_mean_rating[i], ','
        ,ss$Rankings$golden_mean_rank[i], ','
        ,ss$Rankings$predictor_rating[i], ','
        ,ss$Rankings$predictor_rank[i], ','
        ,ss$Rankings$recent_rating[i], ','
        ,ss$Rankings$recent_rank[i], ");"
        )
      )
    }
  
  foreach(i = 1: nrow(ss$Rankings)) %do% {
    updateSagarin(i)
  }
  
}



