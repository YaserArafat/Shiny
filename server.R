library(dygraphs)
library(datasets)
library(RODBC)
library(RMySQL)
library(psych)
library(xts)
library(ggvis)

shinyServer(function(input, output,session) {

  
 

  #---------------------------------------------------------------------------------------------------

  #---------------------------------------------------------------------------------------------------

  
  # ................................... Dashboard ..............................................

  # app started.
  actionsCount <-actionsCount(session)
  impCount <- impressionsCount(session)
  used <- usedBudget(session)
  campTable <- campaignsTable(session)
  appTable <- applicationsTable(session)
  adTable <- adsTable(session)
  activeCampCount <- activeCampaignsCount(session)
  activeAppCount <- activeApplicationsCount(session)
  startTime <- as.numeric(Sys.time())
  eventsCount <- eventsCount(session)
  countryCount <- countryImpressionsCount(session)
  activeAdsCount <- activeAdsCount(session)
  pendingAdsCount <- pendingAdsCount(session)
  pendingAdTable <- pendingAdsTable(session)
  activeAdTable <- activeAdsTable(session)
  output$total_applications <- renderValueBox({
    valueBox(
      value = activeAppCount(),
      subtitle = "Today Total Active Applications(have summaries)",
      icon = icon("area-chart"),
      color = "aqua"
    )
  })
  output$total_campaigns <- renderValueBox({
    valueBox(
      value =activeCampCount(),
      subtitle = "Today Total Active Campaigns",
      icon = icon("area-chart"),
      color = "yellow"
    )
  })
  output$usedBudget <- renderValueBox({
    valueBox(
      value = round(used(),5),
      subtitle = "Today Total Used Budget",
      icon = icon("dollar"),
      color = "aqua"
    )
  })
  
  output$impressions <- renderValueBox({
    valueBox(
      value = impCount(),
      subtitle = "Today Total Impressions",
      icon = icon("area-chart"),
      color = "red"
    )
  })
  
  output$actions <- renderValueBox({
    valueBox(
      actionsCount(),
      "Today Total Clicks",
      icon = icon("area-chart"),
      color = "yellow"
    )
  })
  output$CTR <- renderValueBox({
    valueBox(
      round(actionsCount()/impCount(), digits = 5),
      "CTR Rate",
      icon = icon("percent"),
      color = "blue"
    )
  })
  
  
  
  output$campaignsTable <- DT::renderDataTable(DT::datatable(options = list(dom = 't'),{
    campaignsData <- campTable()
    campaignsData$id = NULL
    campaignsData$day = NULL
    campaignsData$original_daily_budget = NULL
    campaignsData
    
  }))
  output$applicationsTable <- DT::renderDataTable(DT::datatable(options = list(dom = 't'),{
    applicationsData <- appTable()
    applicationsData$id = NULL
    applicationsData$day = NULL
    applicationsData
    
  }))
  output$adsTable <- DT::renderDataTable(DT::datatable(options = list(dom = 't'),{
    adsData <- adTable()
    adsData$id = NULL
    adsData$day = NULL
    adsData
    
  }))

  output$eventsPie <-renderPlot({
    eventsData <- eventsCount()
    if(is.data.frame(eventsData) && is.na(eventsData[1,]))
      return()
    else
    {
    slices <- eventsData$event_count
    lbls <- eventsData$event_id
    piepercent<- round(100*slices/sum(slices), 1)
    
    pie(slices, labels = piepercent, main="Events Percentage over all ads in the system for today",col = rainbow(length(slices)))
    
    legend("topleft",c("Event-1","Event-2","Event-3","Event-4","Event-5"), cex = 0.8, fill = rainbow(length(lbls)))
    }
    
  })
  output$countriesPie <-renderPlot({
    countriesData <- countryCount()
    if(is.data.frame(countriesData) && is.na(countriesData[1,]))
      return()
    else
    {
    print(countriesData)
    lbls <- c("USA","Palestine","KSA")
    slices <- as.numeric(countriesData[1,])
    print(slices)
    piepercent<- round(100*slices/sum(slices), 1)
    
    pie(slices, labels = piepercent, main="Impressions Percentage over all ads in the system for today",col = rainbow(length(slices)))
    legend("topleft",c("USA","Palestine","KSA"), cex = 0.8, fill = rainbow(length(lbls)))
    }  
  })
  # ................................... Applications ............................................
  
  
  output$applicationId <- renderUI({
    
    ss <- as.Date(input$appStart)
    
    sql4 <- sprintf("select distinct(application_id) from application_summaries where day >= '%s' order by application_id desc",ss)
    withProgress(message = 'Making inputs', value = 3, {
      data4 <- dbGetQuery(conn = con, statement =sql4)
    })
    selectInput("applicationId",
                "Application Id:",
                c("All",data4))
  })
  
  output$applicationsSummariesTable <- DT::renderDataTable(DT::datatable({
    s <- as.Date(input$appStart)
    app <- as.integer(input$applicationId)
    
    sql6 <- sprintf("select * from `application_summaries` where  day >= '%s' order by application_id desc",s)
    withProgress(message = 'Making inputs', value = 3, {
      myData <- dbGetQuery(conn = con, statement =sql6)
      
    })
    
    
    if (input$applicationId != "All") {
      
      myData <- myData[myData$application_id == app,]
    }
    myData$id = NULL
    myData
    
  }))
  
  output$appAdTable <- DT::renderDataTable(DT::datatable({
    s <- as.Date(input$appStart)
    app <- as.integer(input$applicationId)
    
    sql6 <- sprintf("select * from `adappcrossing` where  date >= '%s' order by appId desc",s)
    withProgress(message = 'Making inputs', value = 3, {
      myData <- dbGetQuery(conn = reachConnection, statement =sql6)
      
    })
    
    
    if (input$applicationId != "All") {
      
      myData <- myData[myData$appId == app,]
    }
    myData$id = NULL
    myData
    
  }))
  
  # ................................... Lookups ................................................
  output$lkp <- renderUI({
    lookupsSql <- sprintf("select distinct(category) FROM lookups")
  withProgress(message = 'Making inputs', value = 3, {
    lookupsData <- dbGetQuery(conn = reachConnection, statement =lookupsSql)
  })
  selectInput("lkp",
              "Target Category",
              c("All",lookupsData))
  
  })
  
  # Filter data based on selections
  output$categoryItemsTable <- DT::renderDataTable(DT::datatable({
   
    cat <- as.character.POSIXt(input$lkp)
    
    categoryItemsSql <- sprintf("SELECT * FROM lookups where category = '%s' ",cat)
    withProgress(message = 'Making inputs', value = 3, {
      myData <- dbGetQuery(conn = reachConnection, statement =categoryItemsSql)
      print(myData)
    })
    
    if (input$lkp != "All") {
      
      myData <- myData[myData$category == cat,]
    }
    myData$loc = NULL
    myData$id = NULL
    myData
  })
  )
  
  # Filter data based on selections
  output$countriesTable <- DT::renderDataTable(DT::datatable({
    
    countriesSql <- sprintf("SELECT * FROM mcountry")
    withProgress(message = 'Making inputs', value = 3, {
      myData <- dbGetQuery(conn = reachConnection, statement =countriesSql)
    })
    myData$col = NULL
    myData
  })
  )
  
  # ................................... Campaigns ..............................................
  
  sql2 <- sprintf("select name from applications where status = 1 order by id desc")
  sql3 <- sprintf("select name from ads where status = 1 order by id desc")
 
 
   withProgress(message = 'Making inputs', value = 3, {
    data2 <- dbGetQuery(conn = con, statement =sql2)
    
    data3 <- dbGetQuery(conn = con, statement =sql3)
  })
 

  
  output$campaignId <- renderUI({
    
    s <- as.Date(input$campStart)
   
    sql4 <- sprintf("select distinct(campaign_id) from campaign_summaries where day >= '%s' order by id desc",s)
    withProgress(message = 'Making inputs', value = 3, {
    data4 <- dbGetQuery(conn = con, statement =sql4)
    })
    selectInput("campaignId",
                "Campaign Id:",
                c("All",data4))
  })
  
  output$dygraph1 <- renderDygraph({
    
    s <- as.Date(input$campStart)
    c <- as.integer(input$campaignId)
  
   
    print("**************")
    print(c)
    
    if (!is.na(c)) { 
      sql <- sprintf("SELECT day,daily_budget,Used_budget FROM campaign_summaries where day >= '%s' and campaign_id = '%d' group by day", s,c)
      print(sql)
    }
    else
    {
      sql <- sprintf("SELECT day,sum(daily_budget) as daily_budget ,sum(Used_budget) as Used_budget FROM campaign_summaries where day >= '%s' group by day", s) 
      print(sql)
    }
    withProgress(message = 'Making inputs', value = 3, {
    result2 <- dbGetQuery(conn = con, statement =sql)
    })
    if (nrow(result2) != 0){
      
      result2 <- xts(result2[,-1],order.by=as.POSIXct(result2$day))
      dygraph(result2,main = "Daily Budget Vs Used Budget") %>%
        dyOptions(stackedGraph = FALSE)%>%
        dyRangeSelector(height = 0)
    }
    else {
      print('No Data Available')
      return()
      
    }
    
    
    
  })
  
  output$dygraph2 <- renderDygraph({
    s <- as.Date(input$campStart)
    c <- as.integer(input$campaignId)
    if (!is.na(c)) {
      sql4 <- sprintf("SELECT ad_action_summaries.day as day,sum(ad_summaries.impressions) as impressions,sum(ad_action_summaries.impressions) as actions
                      FROM ad_action_summaries
                      join
                      ad_summaries 
                      on
                      ad_action_summaries.ad_id = ad_summaries.ad_id and ad_action_summaries.day = ad_summaries.day
                      where ad_summaries.campaign_id = '%d' and ad_summaries.day >= '%s'
                      group by day",c,s)
      print(sql4)
    }
    else
    {
      sql4 <- sprintf("SELECT ad_action_summaries.day as day,sum(ad_summaries.impressions) as impressions,sum(ad_action_summaries.impressions) as actions
                      FROM ad_action_summaries
                      join
                      ad_summaries 
                      on
                      ad_action_summaries.ad_id = ad_summaries.ad_id and ad_action_summaries.day = ad_summaries.day
                      where ad_summaries.day >= '%s'
                      group by day",s)
      print(sql4)
    }
    withProgress(message = 'Making inputs', value = 3, {
    result3 <- dbGetQuery(conn = con, statement =sql4)
    })
    print("result3:")
    print(result3)
    if (nrow(result3) != 0){
      print("result3:")
      result3 <- xts(result3[,-1],order.by=as.POSIXct(result3$day))
      dygraph(result3,main = "Impressions Vs Actions") %>%
        dyOptions(stackedGraph = FALSE)%>%
        dyRangeSelector(height = 0)
      
    }
    else {
      print('No Data Available')
      return()
      
    }
    
    
  })
  
  output$dygraph3 <- renderDygraph({
    
    s <- as.Date(input$campStart)
    c <- as.integer(input$campaignId)
    if (!is.na(c)) {
      sql5 <- sprintf("select day,sum(`impressions`) as number_of_impressions from `campaign_summaries` where campaign_id = '%d' and day >= '%s group by day",c,s)
      print('sql5')
      print(sql5)
    }
    else
    {
      sql5 <- sprintf("select day,sum(`impressions`) as number_of_impressions from `campaign_summaries` where day >= '%s' group by day",s) 
      print('sql5')
      print(sql5)
    }
    withProgress(message = 'Making inputs', value = 10, {
    result4 <- dbGetQuery(conn = con, statement =sql5)
    })
    result4 <- xts(result4[,-1],order.by=as.POSIXct(result4$day))
    print(result4)
    dygraph(result4,main = "Campaign Summaries") %>%
      dyOptions(stackedGraph = FALSE)%>%
      dyRangeSelector(height = 0)
    
    
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    s <- as.Date(input$campStart)
    c <- as.integer(input$campaignId)
    
    sql6 <- sprintf("select * from `campaign_summaries` where  day >= '%s' ",s)
    withProgress(message = 'Making inputs', value = 3, {
      myData <- dbGetQuery(conn = con, statement =sql6)
      
    })
   
    
    if (input$campaignId != "All") {
      
      myData <- myData[myData$campaign_id == c,]
    }
    myData$id = NULL
    myData
    
  }))
  # ...........................................Ads.................................................
  output$adId <- renderUI({
    s <- as.Date(input$adStart)
    sql4 <- sprintf("select distinct(ad_id) from ad_summaries where day >= '%s' order by id desc",s)
    data4 <- dbGetQuery(conn = con, statement =sql4)
    
    selectInput("adId",
                "Ad Id:",
                c("All",data4))
  })
  
  output$total_active_ads <- renderValueBox({
    valueBox(
      value = activeAdsCount(),
      subtitle = "Today Total Active Ads(have summaries)",
      icon = icon("area-chart"),
      color = "aqua"
    )
  })
    output$total_pending_ads <- renderValueBox({
      valueBox(
        value = pendingAdsCount(),
        subtitle = "Today Total Pending Ads",
        icon = icon("area-chart"),
        color = "aqua"
      )
    })
    output$pendingAdsTable <- DT::renderDataTable(DT::datatable(options = list(dom = 't'),{
      pendingAdsData <- pendingAdTable()
      pendingAdsData$id = NULL
      pendingAdsData
      
    }))
    output$activeAdsTable <- DT::renderDataTable(DT::datatable(options = list(
      dom = 't'
    ),{
      activeAdsData <- activeAdTable()
     
      activeAdsData
      
    }))
    
    output$c <-renderPlot({
      
      s <- as.Date(input$adStart)
      a <- as.integer(input$adId)
      
      if (!is.na(a)) { 
        csql <- sprintf("SELECT * FROM country where id='%d' and date='%s'", a,s)
        withProgress(message = 'Making inputs', value = 3, {
          cresult <- dbGetQuery(conn = reachConnection, statement =csql)
        })
        cresult <- cresult[,which(!apply(cresult,2,FUN = function(x){all(x == 0)}))]
      
      }
      else
      {
        csql <- sprintf("SELECT * FROM country where date='%s'", s) 
        withProgress(message = 'Making inputs', value = 3, {
          cresult <- dbGetQuery(conn = reachConnection, statement =csql)
        })
        cresult <- cresult[,which(!apply(cresult,2,FUN = function(x){all(x == 0)}))]
        #result[,which(!apply(result,2,FUN = function(x){all(x == 0)}))]
      }
     
      print("Result")
      
      print(cresult)
      if(is.data.frame(cresult) && nrow(cresult) < 1)
        return()
      else
      {
        cresult$Id = NULL
        cresult$date = NULL
        lbls <- colnames(cresult)
        print("lbls")
        print(lbls)
        
        print("slices")
       
        slices <- colSums(cresult)
        
       print(slices)
        piepercent<- round(100*slices/sum(slices), 1)
        
        pie(slices, labels = piepercent, main="Impressions Vs Country",col = rainbow(length(slices)))
        legend("topleft",lbls, cex = 0.8, fill = rainbow(length(lbls)))
      }  
      
    })
    output$d <-renderPlot({
      
      s <- as.Date(input$adStart)
      a <- as.integer(input$adId)
      
      if (!is.na(a)) { 
        dsql <- sprintf("SELECT * FROM device where id='%d' and date='%s'", a,s)
        withProgress(message = 'Making inputs', value = 3, {
          dresult <- dbGetQuery(conn = reachConnection, statement =dsql)
        })
        dresult <- dresult[,which(!apply(dresult,2,FUN = function(x){all(x == 0)}))]
      }
      else
      {
        dsql <- sprintf("SELECT * FROM device where date='%s'", s) 
        withProgress(message = 'Making inputs', value = 3, {
          dresult <- dbGetQuery(conn = reachConnection, statement =dsql)
        })
        dresult <- dresult[,which(!apply(dresult,2,FUN = function(x){all(x == 0)}))]
       # result[,which(!apply(result,2,FUN = function(x){all(x == 0)}))]
      }
     
      print("Result")
      
      print(dresult)
      if(is.data.frame(dresult) && nrow(dresult) < 1)
        return()
      else
      {
        dresult$Id = NULL
        dresult$date = NULL
        lbls <- colnames(dresult)
        print("lbls")
        print(lbls)
        print("slices")
        slices <- colSums(dresult)
        print(slices)
        piepercent<- round(100*slices/sum(slices), 1)
        pie(slices, labels = piepercent, main="Impressions Vs device",col = rainbow(length(slices)))
        legend("topleft",lbls, cex = 0.8, fill = rainbow(length(lbls)))
      }  
      
    })
    output$l <-renderPlot({
      
      s <- as.Date(input$adStart)
      a <- as.integer(input$adId)
      
      if (!is.na(a)) { 
        lsql <- sprintf("SELECT * FROM language where id='%d' and date='%s'", a,s)
        withProgress(message = 'Making inputs', value = 3, {
          lresult <- dbGetQuery(conn = reachConnection, statement =lsql)
        })
        lresult <- lresult[,which(!apply(lresult,2,FUN = function(x){all(x == 0)}))]
      }
      else
      {
        lsql <- sprintf("SELECT * FROM language where date='%s'", s) 
        withProgress(message = 'Making inputs', value = 3, {
          lresult <- dbGetQuery(conn = reachConnection, statement =lsql)
        })
        result <- lresult[,which(!apply(lresult,2,FUN = function(x){all(x == 0)}))]
      }
      
      print("Result")
      
      print(lresult)
      if(is.data.frame(lresult) && nrow(lresult) < 1)
        return()
      else
      {
        lresult$Id = NULL
        lresult$date = NULL
        lbls <- colnames(lresult)
        print("lbls")
        print(lbls)
        
        print("slices")
        
        slices <- colSums(lresult)
        
        print(slices)
        piepercent<- round(100*slices/sum(slices), 1)
        
        pie(slices, labels = piepercent, main="Impressions Vs language",col = rainbow(length(slices)))
        legend("topleft",lbls, cex = 0.8, fill = rainbow(length(lbls)))
      }  
      
    })
    
    output$a <-renderPlot({
      
      s <- as.Date(input$adStart)
      a <- as.integer(input$adId)
      
      if (!is.na(a)) { 
        asql <- sprintf("SELECT * FROM age where id='%d' and date='%s'", a,s)
        withProgress(message = 'Making inputs', value = 3, {
          aresult <- dbGetQuery(conn = reachConnection, statement =asql)
        })
        aresult <- aresult[,which(!apply(aresult,2,FUN = function(x){all(x == 0)}))]
      }
      else
      {
        asql <- sprintf("SELECT * FROM age where date='%s'", s) 
        withProgress(message = 'Making inputs', value = 3, {
          aresult <- dbGetQuery(conn = reachConnection, statement =asql)
        })
        aresult <- aresult[,which(!apply(aresult,2,FUN = function(x){all(x == 0)}))]
      }
      
      print("Result")
      
      print(aresult)
      if(is.data.frame(aresult) && nrow(aresult) < 1)
        return()
      else
      {
        aresult$Id = NULL
        aresult$date = NULL
        lbls <- colnames(aresult)
        print("lbls")
        print(lbls)
        
        print("slices")
        
        slices <- colSums(aresult)
        
        print(slices)
        piepercent<- round(100*slices/sum(slices), 1)
        
        pie(slices, labels = piepercent, main="Impressions Vs age",col = rainbow(length(slices)))
        legend("topleft",lbls, cex = 0.8, fill = rainbow(length(lbls)))
      }  
      
    })
    
  # ............................................................................................
  
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    cons <- dbListConnections(MySQL())
    for(con in cons)
    dbDisconnect(con)
    print("Session ended")
  })

  
 
  
})