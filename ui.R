library(dygraphs)
library(shinydashboard)

tags$head(
  tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                  
                  h1 {
                  font-family: 'Lobster', cursive;
                  font-weight: 500;
                  line-height: 1.1;
                  color: #48ca3b;
                  }
                  
                  "))
  )

                    
  sidebar <- dashboardSidebar(

  
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
             badgeColor = "green" ),
      menuSubItem("Campaigns", tabName = "campaigns"),
      menuSubItem("Ads", tabName = "ads"),
      menuSubItem("Applications", tabName = "applications"),
      menuSubItem("Advertisers", tabName = "advertisers"),
      menuSubItem("Publishers", tabName = "publishers"),
    menuItem("Meta Data", icon = icon("bar-chart-o"),
      menuSubItem("Lookups", tabName = "Lookups"),
      menuSubItem("Countries", tabName = "Countries")
    ),
    menuItem("Main Mixberry Site", icon = icon("file-code-o"),
      href = "https://www.mixberrymedia.com"
    )
  )
)

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    
    tabItem("campaigns",
            h1("Campaigns Statistics"),
            fluidRow(
              column(4,dateInput('campStart', "Start date", value = Sys.Date()-7 ,max =Sys.Date())),
              column(4,uiOutput("campaignId"))
            ),

            fluidRow( 
              column(6,dygraphOutput("dygraph1", height = 400)),
              
              column(6,dygraphOutput("dygraph2", height = 400))
            ),
            br(),
            br(),
            fluidRow(
              DT::dataTableOutput("table")
            ),
            br(),
            br()
            
            
    ),
#---------------------------------------------------------------------------------------------------------------------------
    tabItem("ads",
            h1("Ads Statistics"),
            fluidRow(
              valueBoxOutput("total_active_ads"),
              valueBoxOutput("total_pending_ads")
              
            ),
            box(
              title = "Pending Ads",
              status = "primary",
              DT::dataTableOutput("pendingAdsTable")
                    
              ),
            box(
              title = "Recent Created Active Ads",
              status = "primary",
              DT::dataTableOutput("activeAdsTable")
              
            )
            ,
            fluidRow(
              column(4,dateInput('adStart', "Select date", value = Sys.Date() ,max =Sys.Date())),
              column(4,uiOutput("adId"))
            ),
            
            fluidRow(
              box(
                
                title = "Ad Country Distribution",
                status = "primary",
                background = "aqua",
                plotOutput("c")
                
                
                
              ),
              box(
                
                title = "Ad Age Distribution",
                status = "primary",
                background = "aqua",
                plotOutput("a")
                
                
                
              ),
              box(
                
                title = "Ad Device Distribution",
                status = "primary",
                background = "aqua",
                plotOutput("d")
                
                
                
              ),
              box(
                
                title = "Ad Language Distribution",
                status = "primary",
                background = "aqua",
                plotOutput("l")
                
                
                
              )
              
            )   
            
            
    ),
#----------------------------------------------Applications----------------------------------------------------------------------------------

    tabItem("applications",
            h1("Applications Statistics"),
            fluidRow(
              column(4,dateInput('appStart', "Start date", value = Sys.Date()-7 ,max =Sys.Date())),
              column(4,uiOutput("applicationId"))
            ),
            h2("Applications Summaries",align = "center",
               style = "font-weight: 300; line-height: 1.1; 
               color: black;"),
            tabPanel("Applications Summaries",
                     DT::dataTableOutput("applicationsSummariesTable")),
            h2("Applications Vs Ads",align = "center",
               style = "font-weight: 300; line-height: 1.1; 
               color: black;"),
            tabPanel("Applications Vs Ads",
                     DT::dataTableOutput("appAdTable"))
            
    ),
   #--------------------------------------------Advertisers----------------------------------------------------------------------------------
    
    tabItem("advertisers",
            h1("Advertisers Statistics")
            
    ),
    tabItem("publishers",
            h1("Publishers Statistics")
            
    ),

   #----------------------------------------------Lookups----------------------------------------------------------------------------------

    tabItem("Lookups",
        h1("Lookups"),
        uiOutput("lkp"),
        tabPanel("Category Items",
                 DT::dataTableOutput("categoryItemsTable")
        )
        
),
tabItem("Countries",
        h1("Countries"),
        tabPanel("Supported Countries",
                 DT::dataTableOutput("countriesTable")
        )
        
),
#----------------------------------------------DASHBOARD----------------------------------------------------------------------------------
    tabItem("dashboard",
            fluidRow(
              valueBoxOutput("total_applications"),
              valueBoxOutput("total_campaigns"),
              valueBoxOutput("impressions"),
              valueBoxOutput("usedBudget"),
              valueBoxOutput("actions"),
              valueBoxOutput("CTR")
              
             
            ),
      
      
      fluidRow(
        box(
     
          title = "Events Distribution",
          status = "primary",
          background = "aqua",
          plotOutput("eventsPie")
          
         
        
        ),
        box(
          
          title = "Country Distribution",
          status = "primary",
          background = "aqua",
          plotOutput("countriesPie")
          
          
          
        )
       
          
         
        
      ),
      
      fluidRow(
      tabBox(width = 12,
        
        tabPanel("Most Active Campaigns",
                 DT::dataTableOutput("campaignsTable")
        ),
        tabPanel("Most Active Applications",
                 DT::dataTableOutput("applicationsTable")
        ),
        tabPanel("Most Active Ads",
                 DT::dataTableOutput("adsTable")
        )
      )
      ) 
      
      
      
    )
  )
)

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------


messages <- dropdownMenu(type = "messages",
  messageItem(
    from = "Sales Dept",
    message = "Sales are steady this month."
  ),
  messageItem(
    from = "New User",
    message = "How do I register?",
    icon = icon("question"),
    time = "13:45"
  ),
  messageItem(
    from = "Support",
    message = "The new server is ready.",
    icon = icon("life-ring"),
    time = "2014-12-01"
  )
)

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------


notifications <- dropdownMenu(type = "notifications", badgeStatus = "warning",
  notificationItem(
    text = "5 new users today",
    icon("users")
  ),
  notificationItem(
    text = "12 items delivered",
    icon("truck"),
    status = "success"
  ),
  notificationItem(
    text = "Server load at 86%",
    icon = icon("exclamation-triangle"),
    status = "warning"
  )
)

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

tasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
  taskItem(value = 90, color = "green",
    "Documentation"
  ),
  taskItem(value = 17, color = "aqua",
    "Project X"
  ),
  taskItem(value = 75, color = "yellow",
    "Server deployment"
  ),
  taskItem(value = 80, color = "red",
    "Overall project"
  )
)

header <- dashboardHeader(
  title = "Mixberry Dashboard",
  messages,
  notifications,
  tasks
)

ui <- dashboardPage(header, sidebar, body)
  
