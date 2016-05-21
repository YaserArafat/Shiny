library(shiny)
library(dplyr)
library(shinydashboard)


con <- dbConnect(MySQL(),
                 user = '*******',
                 password = '*******',
                 host = '*******',
                 dbname='*******')
con_p <- dbConnect(MySQL(),
                 user = '*******',
                 password = '*******',
                 host = '*******',
                 dbname='*******')

trackingCon <- dbConnect(MySQL(),
                 user = '*******',
                 password = '*******',
                 host = '*******',
                 dbname='*******')

reachConnection <- dbConnect(MySQL(),
                 user = '*******',
                 password = '*******',
                 host = '*******',
                 dbname='*******')


s <- Sys.Date()

countryImpressionsCount <- function(session) {
  
  reactive({
    
    sql <- sprintf("select sum(c49)as USA,sum(c56) as Palestine,sum(c40) as KSA
FROM MBLogAnalysis_41.country where date = '%s'",s)
    total <- dbGetQuery(conn = reachConnection, statement =sql)
    total
  })
}

eventsCount <- function(session) {
  
  reactive({
    
    invalidateLater(5000, session)
    sql <- sprintf("SELECT event_id,sum(impressions)  as event_count from events where day = '%s' group by event_id",s)
    total <- dbGetQuery(conn = trackingCon, statement =sql)
    total
  })
}

impressionsCount <- function(session) {

  reactive({
  
    invalidateLater(5000, session)
    sql6 <- sprintf("select sum(impressions) from campaign_summaries where day = '%s'",s)
    total1 <- dbGetQuery(conn = con, statement =sql6)
    total1
  })
}
actionsCount <- function(session) {
  
  reactive({
    
    invalidateLater(5000, session)
    sql7 <- sprintf("select sum(impressions) from ad_action_summaries where day = '%s'",s)
    total2 <- dbGetQuery(conn = con, statement =sql7)
    total2
  })
}
usedBudget <- function(session) {
    
    reactive({

      invalidateLater(5000, session)
      sql8 <- sprintf("select sum(used_budget) from campaign_summaries where day = '%s'",s)
      total3 <- dbGetQuery(conn = con, statement =sql8)
      total3
    })

}
campaignsTable <- function(session) {
  
  reactive({
    
    invalidateLater(20000, session)
    sql9 <- sprintf("select * from campaign_summaries  where day = '%s' order by impressions desc limit 8",s)
    total4 <- dbGetQuery(conn = con, statement =sql9)
    total4
    
  })
  
}
applicationsTable <- function(session) {
  
  reactive({
    invalidateLater(20000, session)
    sql10 <- sprintf("select * from application_summaries where day = '%s' order by impressions desc limit 8",s)
    total5 <- dbGetQuery(conn = con, statement =sql10)
    total5
    
  })
}
adsTable <- function(session) {
    
    reactive({
      invalidateLater(20000, session)
      sql11 <- sprintf("select * from ad_summaries where day = '%s' order by impressions desc limit 8",s)
      total6 <- dbGetQuery(conn = con, statement =sql11)
      total6
      
    })
    
}
activeCampaignsCount <- function(session) {
  
  reactive({
    sql7 <- sprintf("select count(*) from campaigns where end_time > now() and status = 1")
    total7 <- dbGetQuery(conn = con, statement =sql7)
    total7
  })
} 
activeApplicationsCount <- function(session) {
  
  reactive({
    sql8 <- sprintf("SELECT count(*) FROM application_summaries where impressions > 0 and day = '%s'",s)
    total8 <- dbGetQuery(conn = con, statement =sql8)
    total8
  })
}
activeAdsCount <- function(session) {
  
  reactive({
    sql9 <- sprintf("SELECT count(*) FROM ads")
    total9 <- dbGetQuery(conn = con_p, statement =sql9)
    total9
  })
}
pendingAdsCount <- function(session) {
  
  reactive({
    sql10 <- sprintf("select count(*) from ads 
join campaigns on
                     ads.campaign_id = campaigns.id where ads.status=0 and campaigns.status=1")
    total10 <- dbGetQuery(conn = con, statement =sql10)
    total10
  })
}
pendingAdsTable <- function(session) {
  
  reactive({
    sql11 <- sprintf("select ads.id as ID,ads.name as Ad_NAME,campaigns.name as CAMPAIGN_NAME from ads 
                     join campaigns on
                     ads.campaign_id = campaigns.id where ads.status=0 and campaigns.status=1")
    total11 <- dbGetQuery(conn = con, statement =sql11)
    total11
  })
}
activeAdsTable <- function(session) {
  
  reactive({
    sql12 <- sprintf("SELECT id,campaign_id,message_type FROM ads order by id desc limit 5")
    total12 <- dbGetQuery(conn = con_p, statement =sql12)
    total12
  })
}


