library(shiny)
library(tidyverse)
library(flexdashboard)
library(shinydashboard)



dashboardPage( dashboardHeader(title = "S.A Rent"),
               
              dashboardSidebar(disable = TRUE),
              
              dashboardBody ( fluidPage ( br(),
          
          # Add title panel and icon 
          titlePanel(title = div(img(src= "south african flag.jpg", height=50, width=100), "Rent in 5 South African Cities")),
          
          p("This project uses webscraped data from property.mg.co.za to analyze rent variation in 5 South african cities"),
          
          # Create a city selection input for user
          
          fluidRow(style = "background-color: light-blue; color:black",
                    column(12,
                           wellPanel(selectInput("city_list",
                                                     "Select City:",
                                                     choices = c("All", "Cape Town", "Pretoria", "Johannesburg", "Soweto", "East London"))
                                      )
                           )
                  ),
          
          # Add second row to display valuetextbox
          
          fluidRow(
                    valueBoxOutput("avg_rent"),
                    valueBoxOutput("min_rent"),
                    valueBoxOutput("max_rent")
                  ),
          
          # Add 3rd row for Histogram and top 10 cheapest accommodation and location
          
          fluidRow( style = "background-color: light-blue; color:black",
                    column(7, wellPanel(plotOutput("hist_plot"))),
                    column(5, wellPanel(plotOutput("table_cheap"))) 
                  ),
          
          # Add 4th row for box plot and top 10 cheapest accommodation and location
          
          fluidRow( style = "background-color: light-blue; color:black",
                    column (7,wellPanel(plotOutput("box_plot"))),
                    column(5, wellPanel(plotOutput("table_expensive"))) 
                  )

        ) )
        )