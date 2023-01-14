library(shiny)
library(tidyverse)
library(flexdashboard)
library(shinydashboard)
library(scales)


airline_list <- unique(new_flight_df$uniquecarrier)
month_list <- unique(new_flight_df$month_name)

dashboardPage(dashboardHeader(title = "Flight Dashboard", titleWidth = 200),
              dashboardSidebar( 
                                # Side bar of the dashboard
                
                                selectInput( inputId = "carrier",
                                             label = "Select Carrier:",
                                             choices = c("All Carrier", airline_list),
                                             selected = "All Carrier",
                                             selectize = FALSE),
                                
                                # Side menu of the dashboard
                                
                                sidebarMenu( selectInput( inputId = "month",
                                                          label = "Select Month:",
                                                          choices = c("All Year",month_list),
                                                          selected = "All Year"))
                                ),
              
              dashboardBody( fluidPage( br(),
                                        titlePanel(title = div(img(src = "plane.jpg", height = 50, width = 100), "Flight Delay Analysis")),
                                        
                                        p("This analysis was conducted to see pattern of delay in flights by different carriers in 2008. Data Source: Kaggle"),
                                        
                                        fluidRow( style = "background-color: light-blue; color:black",
                                                 valueBoxOutput("total_flight"),
                                                 valueBoxOutput("percent_delayed"),
                                                 valueBoxOutput("delay_duration")),
                                        
                                        fluidRow(style = "background-color: light-blue; color:black",
                                                 column(7, wellPanel(plotOutput("heatmap"))),
                                                 column(5, wellPanel(plotOutput("routedelay")))),
                                        
                                        fluidRow(style = "background-color: light-blue; color:black",
                                                 column(7,wellPanel(plotOutput("totalflight"))),
                                                 column(5, wellPanel(plotOutput("carrierdelay"))))
                                        
                                        
                                        
                                        )))




