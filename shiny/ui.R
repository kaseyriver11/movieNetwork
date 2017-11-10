


# Start Application
shinyUI(
    bootstrapPage(
        fluidPage(
            theme = "bootstrap.css",
            
            
            navbarPage("Film Network Analysis"),
            # Panel 1
            sidebarLayout(
                sidebarPanel(width = 4,
                             
                             fluidRow(
                                 column(6,selectInput("cast_crew", "Cast or Crew:",
                                                      choices = c("cast" = "cast", "crew" = "crew"),
                                                      selected = "cast"))
                             ),
                             
                                 uiOutput("genres"),
                                 
                                 fluidRow(
                                     column(6,numericInput("movie_count", "Number of Movies:",
                                                           20, min = 20, max = 300)),
                                     column(6,numericInput("actor_count", "Number of People:",
                                                           7, min = 1, max = 20))
                                 ),
                                 
                                 dateRangeInput('dateRange',
                                                label = 'Date range for Movies:',
                                                start = "2010/01/01", end = "2015/12/31"
                                 ),
                                 
                                 actionButton("goButton", "Find Movies:"),
                             
                             br(),
                             
                             uiOutput("actors")
                ),
                
                mainPanel(
                    
                    tabsetPanel(
                        tabPanel("Network Diagram",
                                 forceNetworkOutput("force", height = "650px", width = "110%")),
                        
                        tabPanel("Most Influencial Cast/Crew",
                                 fluidPage(
                                     fluidRow(
                                         column(1),
                                         column(10, dataTableOutput("recentTable1"))
                                     )))
                    )
                )
            )
        )
))

