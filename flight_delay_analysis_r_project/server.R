library(shiny)
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(rio)
library(ggthemes)
library(fontawesome)
library(shinydashboard)
library(reshape2)  

shinyServer(function(input,output){
  
  # Import data set
  
  flight_db <- import("clean_flight_delay.csv")
  
  
  # Display name
  
  showNotification("Created by Chukwudi Ogbuta", duration = NULL, type = "message")
  
  # Observe event incase carrier or month is selected
  
  observeEvent(c(input$carrier, input$month),{if(input$carrier == "All Carrier" & input$month== "All Year") 

{ # Pass in values into the value outbox
                      
# Calcuate total flight for all carrier
                 
output$total_flight <- renderValueBox({  
                                        all_flights <- flight_db %>% nrow() %>% prettyNum(big.mark = ",")
                                        valueBox("Number of Flights",value = tags$p(all_flights, style= "font-size: 70%;"),color = "red" , icon = icon("plane"))
                                       })
                 
# Calculate percentage of delayed flights for all carriers
                        
output$percent_delayed <- renderValueBox({ delayed <- flight_db %>% 
                                             filter(arrdelay > 0) %>% 
                                             nrow()
                        
                                             all_flights <- flight_db %>% nrow()
                                                                    
                                             delayed_flight_percent = as.integer((delayed/all_flights)*100)
                                                                    
                                             valueBox("Flights Delayed",value = tags$p(paste0(delayed_flight_percent, "%"), style= "font-size: 70%;"),color = "purple", icon = icon("percent") )
                                                                })
                    
# Calculate average delay duration throughout the year
                        
output$delay_duration <- renderValueBox({ avg_delay <- flight_db %>% 
                                            summarise(mean_delay = mean(arrdelay))
                                                                  
                                            valueBox("Average Delay Duration",value = tags$p(paste(as.integer(avg_delay), "minutes"), style= "font-size: 70%;"),color = "maroon", icon = icon("clock") )
                                                                  })
                        
# Display heat map to show average monthly delay by carriers

output$heatmap <- renderPlot({ carrier_monthly_delays <- flight_db %>% 
                                 group_by(uniquecarrier, month_name) %>% 
                                 summarise(avg_monthly_delay = mean(arrdelay)) %>% 
                                 arrange(avg_monthly_delay)

                                carrier_monthly_delays$month_name <- as.factor(carrier_monthly_delays$month_name)
                                levels(carrier_monthly_delays$month_name) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                                
                                carrier_monthly_delays %>% 
                                  ggplot(aes(x=uniquecarrier, y= month_name, fill=avg_monthly_delay))+
                                  geom_tile(color= "white", size=0.15) +
                                  scale_fill_gradient2(low = "white", high = "blue")+
                                  labs(x= "Unique Carrier",
                                       y = "Month",
                                       title = "Average Delay by Carriers Per Month",
                                       fill = "Average Delay") +
                                  theme_wsj()+
                                  theme(axis.text = element_text(size = 9),
                                        plot.title = element_text(size=15, face = "bold", hjust = 0.5),
                                        axis.title = element_text(size = 13, face = "bold"),
                                        legend.text = element_text(size = 10),
                                        legend.title = element_text(size = 11, face = "bold"),
                                        legend.position = "bottom")
                            })

# Bar plot to show routes with the most number of delays

output$routedelay <- renderPlot({ top_10_delayed_route <- flight_db %>% 
                                    group_by(uniquecarrier, route) %>% 
                                    summarise(route_delay = mean(arrdelay)) %>% 
                                    arrange(-route_delay) %>% 
                                    head(10)
                                  
                                  route_plot= melt(top_10_delayed_route[,c("route", "route_delay")], id.vars = 1)
                                  
                                  ggplot(route_plot, aes(x=route, y=value))+
                                    geom_bar(aes(fill= variable), stat="identity", position="dodge")+
                                    labs(x= "Routes",
                                         y = "Frequency of Delay",
                                         title = "Top 10 Routes With The Most Delay",
                                         fill = "Frequency") +
                                    theme_wsj()+
                                    theme(axis.text = element_text(size = 9),
                                          plot.title = element_text(size=14, face = "bold", hjust = 0.5),
                                          axis.title = element_text(size = 13, face = "bold"),
                                          legend.text = element_text(size = 10),
                                          legend.title = element_text(size = 11, face = "bold"),
                                          legend.position = "right") +
                                    coord_flip()
})

# Bar plot to show total number of flights per month

output$totalflight <- renderPlot({ flight_db$month_name <- as.factor(flight_db$month_name)
levels(flight_db$month_name) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                                  flight_db %>% 
                                    ggplot(aes(x=month_name))+
                                    geom_bar (fill="slateblue2")+
                                    labs(x= "Month",
                                         y = "Number of Flights",
                                         title = "Flights Per Month") +
                                    geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "black")+
                                    theme_wsj()+
                                    theme(axis.text = element_text(size = 9),
                                          plot.title = element_text(size=15, face = "bold", hjust = 0.5),
                                          axis.title = element_text(size = 13, face = "bold"))
})

# Bar plot to show top 10 carriers with the longest average delay 

output$carrierdelay <- renderPlot({ avg_carrier_delay_duration <- flight_db %>% 
                                      group_by(uniquecarrier) %>% 
                                      summarize(average_delay = mean(arrdelay)) %>% 
                                      arrange(-average_delay)
                                    
                                    avg_carrier_delay_duration <- head(avg_carrier_delay_duration, 10)
                                    
                                    melt_acdd= melt(avg_carrier_delay_duration[,c("uniquecarrier", "average_delay")], id.vars = 1)
                                    
                                    ggplot(melt_acdd, aes(x=uniquecarrier, y=value))+
                                      geom_bar(aes(fill= variable), stat="identity", position="dodge") +
                                      labs(x= "Carrier",
                                           y = "Duration of Delay",
                                           title = "Top 10 Carriers With The Most Delay",
                                           fill = "Duration") +
                                      theme_wsj()+
                                      theme(axis.text = element_text(size = 9),
                                            plot.title = element_text(size=14, face = "bold", hjust = 0.5),
                                            axis.title = element_text(size = 13, face = "bold"),
                                            legend.text = element_text(size = 10),
                                            legend.title = element_text(size = 11, face = "bold"),
                                            legend.position = "right") +
                                      coord_flip()
                                    
                                    
  
})                                  
                                             
  } else if (input$carrier != "All Carrier" & input$month== "All Year") 
    {
        # Filter data down to rows that contain input carrier 
    
        carrier_select <- reactive({flight_db %>% filter(uniquecarrier == input$carrier)})
        
        # Calculate total flight for specific carrier
        
        output$total_flight <- renderValueBox({ all_flights <- carrier_select() %>%  nrow() %>% prettyNum(big.mark = ",")
          valueBox("Number of Flights",value = tags$p(all_flights, style= "font-size: 70%;"),color = "red", icon = icon("plane")  )
        })
        
        # Calculate percentage of delayed flights for specific carriers
        
        output$percent_delayed <- renderValueBox({ delayed <- carrier_select() %>% 
                                                    filter(arrdelay > 0) %>% 
                                                    nrow()
        
        all_flights <- carrier_select() %>% nrow()
        
        delayed_flight_percent = as.integer((delayed/all_flights)*100)
        
        valueBox("Flights Delayed",value = tags$p(paste0(delayed_flight_percent, "%"), style= "font-size: 70%;"),color = "purple", icon = icon("percent")  )
        })
        
        # Calculate average delay duration throughout the year for specific carrier
        
        output$delay_duration <- renderValueBox({ avg_delay <- carrier_select() %>% 
          summarise(mean_delay = mean(arrdelay))
        
        valueBox("Average Delay Duration",value = tags$p(paste(as.integer(avg_delay), "minutes"), style= "font-size: 70%;"),color = "maroon", icon = icon("clock")  )
        })
        
        # Display heat map to show average monthly delay for specific carrier
        
        output$heatmap <- renderPlot({ carrier_monthly_delays <- carrier_select()  %>% 
          group_by(uniquecarrier, month_name) %>% 
          summarise(avg_monthly_delay = mean(arrdelay)) %>% 
          arrange(avg_monthly_delay)
        
        carrier_monthly_delays$month_name <- as.factor(carrier_monthly_delays$month_name)
        levels(carrier_monthly_delays$month_name) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        
        carrier_monthly_delays %>% 
          ggplot(aes(x=uniquecarrier, y= month_name, fill=avg_monthly_delay))+
          geom_tile(color= "white", size=0.15) +
          scale_fill_gradient2(low = "white", high = "blue")+
          labs(x= "Unique Carrier",
               y = "Month",
               title = paste("Average Delay by", input$carrier, "Carrier Per Month"),
               fill = "Average Delay") +
          geom_text(aes(label = as.integer(avg_monthly_delay)), color = "black") +
          theme_wsj()+
          theme(axis.text = element_text(size = 9),
                plot.title = element_text(size=15, face = "bold", hjust = 0.5),
                axis.title = element_text(size = 13, face = "bold"),
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 11, face = "bold"),
                legend.position = "bottom") +
          coord_flip()
        })
        
        # Bar plot to show routes with the most number of delays for specific carrier
        
        output$routedelay <- renderPlot({ top_10_delayed_route <- carrier_select()  %>% 
          group_by(uniquecarrier, route) %>% 
          summarise(route_delay = mean(arrdelay)) %>% 
          arrange(-route_delay) %>% 
          head(10)
        
        route_plot= melt(top_10_delayed_route[,c("route", "route_delay")], id.vars = 1)
        
        ggplot(route_plot, aes(x=route, y=value))+
          geom_bar(aes(fill= variable), stat="identity", position="dodge")+
          labs(x= "Routes",
               y = "Frequency of Delay",
               title = paste("Top 10 Routes With The Most Delay -", input$carrier ),
               fill = "Frequency") +
          theme_wsj()+
          theme(axis.text = element_text(size = 9),
                plot.title = element_text(size=14, face = "bold", hjust = 0.5),
                axis.title = element_text(size = 13, face = "bold"),
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 11, face = "bold"),
                legend.position = "right") +
          coord_flip()
        })
        
        # Bar plot to show total number of flights per month for specific carrier
        
        output$totalflight <- renderPlot({carrier_select() %>% 
                                              ggplot(aes(x=factor(month_name,ordered = TRUE, levels =  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))))+
                                              geom_bar (fill="slateblue2")+
                                              labs(x= "Month",
                                                   y = "Number of Flights",
                                                   title = paste("Flights Per Month -", input$carrier )) +
                                              geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "black")+
                                              theme_wsj()+
                                              theme(axis.text = element_text(size = 9),
                                                    plot.title = element_text(size=15, face = "bold", hjust = 0.5),
                                                    axis.title = element_text(size = 13, face = "bold"))
        })
        
        # Bar plot to show average delay (specific carrier) 
        
        output$carrierdelay <- renderPlot({ avg_carrier_delay_duration <- carrier_select() %>% 
          group_by(uniquecarrier) %>% 
          summarize(average_delay = mean(arrdelay)) %>% 
          arrange(-average_delay)
        
        avg_carrier_delay_duration <- head(avg_carrier_delay_duration, 10)
        
        melt_acdd= melt(avg_carrier_delay_duration[,c("uniquecarrier", "average_delay")], id.vars = 1)
        
        ggplot(melt_acdd, aes(x=uniquecarrier, y=value))+
          geom_bar(aes(fill= variable), stat="identity", position="dodge") +
          labs(x= "Carrier",
               y = "Duration of Delay",
               title = paste("Average Delay Duration -", input$carrier),
               fill = "Duration") +
          theme_wsj()+
          theme(axis.text = element_text(size = 9),
                plot.title = element_text(size=14, face = "bold", hjust = 0.5),
                axis.title = element_text(size = 13, face = "bold"),
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 11, face = "bold"),
                legend.position = "right") +
          coord_flip()
        
        })     
        
        
    
  } else if (input$carrier == "All Carrier" & input$month!= "All Year") 
  {
    # Filter data down to rows that contain input month 
    
    month_select <- reactive({flight_db %>% filter(month_name == input$month)})
    
    # Calculate total flight for all carrier for specific month
    
    output$total_flight <- renderValueBox({ all_flights <- month_select() %>%  nrow()%>% prettyNum(big.mark = ",")
    valueBox("Number of Flights",value = tags$p(all_flights, style= "font-size: 70%;"),color = "red", icon = icon("plane")  )
    })
    
    # Calculate percentage of delayed flights for all carriers for specific month
    
    output$percent_delayed <- renderValueBox({ delayed <- month_select() %>% 
      filter(arrdelay > 0) %>% 
      nrow()
    
    all_flights <- month_select() %>% nrow()
    
    delayed_flight_percent = as.integer((delayed/all_flights)*100)
    
    valueBox("Flights Delayed",value = tags$p(paste0(delayed_flight_percent, "%"), style= "font-size: 70%;"),color = "purple", icon = icon("percent")  )
    })
    
    # Calculate average delay duration throughout the year for specific month
    
    output$delay_duration <- renderValueBox({ avg_delay <- month_select() %>% 
      summarise(mean_delay = mean(arrdelay))
    
    valueBox("Average Delay Duration",value = tags$p(paste(as.integer(avg_delay), "minutes"), style= "font-size: 70%;"),color = "maroon", icon = icon("clock")  )
    })
    
    # Display heat map to show average weekly delay by carriers for specific month
    
    output$heatmap <- renderPlot({ carrier_weekly_delay <- month_select() %>% 
      group_by(uniquecarrier, weekday) %>% 
      summarise(avg_weekly_delay = mean(arrdelay)) %>% 
      arrange(avg_weekly_delay)
    
    
    carrier_weekly_delay$weekday <- as.factor(carrier_weekly_delay$weekday)
    levels(carrier_weekly_delay$weekday) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    
    carrier_weekly_delay %>% 
      ggplot(aes(x=uniquecarrier, y= weekday, fill=avg_weekly_delay))+
      geom_tile(color= "white", size=0.15) +
      scale_fill_gradient2(low = "white", high = "blue")+
      labs(x= "Unique Carrier",
           y = "Week Days",
           title = paste("Average Delay by Carriers on Weekdays -", input$month),
           fill = "Average Delay") +
      theme_wsj()+
      theme(axis.text = element_text(size = 9),
            plot.title = element_text(size=15, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 13, face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 11, face = "bold"),
            legend.position = "bottom")
    })
    
    # Bar plot to show routes with the most number of delays for specific month
    
    output$routedelay <- renderPlot({ top_10_delayed_route <- month_select()  %>% 
      group_by(uniquecarrier, route) %>% 
      summarise(route_delay = mean(arrdelay)) %>% 
      arrange(-route_delay) %>% 
      head(10)
    
    route_plot= melt(top_10_delayed_route[,c("route", "route_delay")], id.vars = 1)
    
    ggplot(route_plot, aes(x=route, y=value))+
      geom_bar(aes(fill= variable), stat="identity", position="dodge")+
      labs(x= "Routes",
           y = "Frequency of Delay",
           title = paste("Top 10 Routes With The Most Delay -", input$month ),
           fill = "Frequency") +
      theme_wsj()+
      theme(axis.text = element_text(size = 9),
            plot.title = element_text(size=14, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 13, face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 11, face = "bold"),
            legend.position = "right") +
      coord_flip()
    })
    
    # Bar plot to show total number of flights per day of specific month
    
    output$totalflight <- renderPlot({month_select() %>% 
        ggplot(aes(x=factor(dayofmonth)))+
                     geom_bar (fill="slateblue2")+
                     labs(x= "Days",
                          y = "Number of Flights",
                          title = paste("Flights Per Day -", input$month )) +
                     geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "black")+
                     theme_wsj()+
                     theme(axis.text = element_text(size = 9),
                           plot.title = element_text(size=15, face = "bold", hjust = 0.5),
                           axis.title = element_text(size = 13, face = "bold"))
    })
    
    # Bar plot to show carriers with top 10 delay for specific month
    
    output$carrierdelay <- renderPlot({ avg_carrier_delay_duration <- month_select() %>% 
      group_by(uniquecarrier) %>% 
      summarize(average_delay = mean(arrdelay)) %>% 
      arrange(-average_delay)
    
    avg_carrier_delay_duration <- head(avg_carrier_delay_duration, 10)
    
    melt_acdd= melt(avg_carrier_delay_duration[,c("uniquecarrier", "average_delay")], id.vars = 1)
    
    ggplot(melt_acdd, aes(x=uniquecarrier, y=value))+
      geom_bar(aes(fill= variable), stat="identity", position="dodge") +
      labs(x= "Carrier",
           y = "Duration of Delay",
           title = paste("Top 10 Carriers With Most Delay -", input$month),
           fill = "Duration") +
      theme_wsj()+
      theme(axis.text = element_text(size = 9),
            plot.title = element_text(size=14, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 13, face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 11, face = "bold"),
            legend.position = "right") +
      coord_flip()
    
    })     
    
    
  } else{
    # Filter data down to rows that contain input month 
    
    carrier_month_select <- reactive({flight_db %>% filter(month_name == input$month & uniquecarrier == input$carrier)})
    
    # Calculate total flight for that carrier in that month
    
    output$total_flight <- renderValueBox({ all_flights <- carrier_month_select() %>%  nrow() %>% prettyNum(big.mark = ",")
    valueBox("Number of Flights",value = tags$p(all_flights, style= "font-size: 70%;"),color = "red", icon = icon("plane")  )
    })
    
    # Calculate percentage of delayed flights for specific carrier - specific month
    
    output$percent_delayed <- renderValueBox({ delayed <- carrier_month_select() %>% 
      filter(arrdelay > 0) %>% 
      nrow()
    
    all_flights <- carrier_month_select() %>% nrow()
    
    delayed_flight_percent = as.integer((delayed/all_flights)*100)
    
    valueBox("Flights Delayed",value = tags$p(paste0(delayed_flight_percent, "%"), style= "font-size: 70%;"),color = "purple", icon = icon("percent")  )
    })
    
    # Calculate average delay duration throughout the year (specific carrier - specific month)
    
    output$delay_duration <- renderValueBox({ avg_delay <- carrier_month_select() %>% 
      summarise(mean_delay = mean(arrdelay))
    
    valueBox("Average Delay Duration",value = tags$p(paste(as.integer(avg_delay), "minutes"), style= "font-size: 70%;"),color = "maroon", icon = icon("clock")  )
    })
    
    # Display heat map to show average weekly delay (specific carrier - specific month)
    
    output$heatmap <- renderPlot({ carrier_weekly_delay <- carrier_month_select() %>% 
      group_by(uniquecarrier, weekday) %>% 
      summarise(avg_weekly_delay = mean(arrdelay)) %>% 
      arrange(avg_weekly_delay)
    
    
    carrier_weekly_delay$weekday <- as.factor(carrier_weekly_delay$weekday)
    levels(carrier_weekly_delay$weekday) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    
    carrier_weekly_delay %>% 
      ggplot(aes(x=uniquecarrier, y= weekday, fill=avg_weekly_delay))+
      geom_tile(color= "white", size=0.15) +
      scale_fill_gradient2(low = "white", high = "blue")+
      labs(x= "Unique Carrier",
           y = "Week Days",
           title = paste("Average Delay by", input$carrier,  "on Weekdays -", input$month),
           fill = "Average Delay") +
      geom_text(aes(label = as.integer(avg_weekly_delay)), color = "black") +
      theme_wsj()+
      theme(axis.text = element_text(size = 9),
            plot.title = element_text(size=15, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 13, face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 11, face = "bold"),
            legend.position = "bottom") + coord_flip()
    })
    
    # Bar plot to show routes with the most number of delays (specific carrier - specific month)
    
    output$routedelay <- renderPlot({ top_10_delayed_route <- carrier_month_select()  %>% 
      group_by(uniquecarrier, route) %>% 
      summarise(route_delay = mean(arrdelay)) %>% 
      arrange(-route_delay) %>% 
      head(10)
    
    route_plot= melt(top_10_delayed_route[,c("route", "route_delay")], id.vars = 1)
    
    ggplot(route_plot, aes(x=route, y=value))+
      geom_bar(aes(fill= variable), stat="identity", position="dodge")+
      labs(x= "Routes",
           y = "Frequency of Delay",
           title = paste("Top 10 Routes With The Most Delay -", input$carrier ),
           fill = "Frequency") +
      theme_wsj()+
      theme(axis.text = element_text(size = 9),
            plot.title = element_text(size=14, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 13, face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 11, face = "bold"),
            legend.position = "right") +
      coord_flip()
    })
    
    # Bar plot to show total number of flights per day (specific carrier - specific month)
    
    output$totalflight <- renderPlot({carrier_month_select() %>% 
        ggplot(aes(x=factor(dayofmonth)))+
                     geom_bar (fill="slateblue2")+
                     labs(x= "Days",
                          y = "Number of Flights",
                          title = paste("Flights Per Day by",input$carrier, "-", input$month )) +
                     geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "black")+
                     theme_wsj()+
                     theme(axis.text = element_text(size = 9),
                           plot.title = element_text(size=15, face = "bold", hjust = 0.5),
                           axis.title = element_text(size = 13, face = "bold"))
    })
    
    # Bar plot to show average delay (specific carrier - specific month) 
    
    output$carrierdelay <- renderPlot({ avg_carrier_delay_duration <- carrier_month_select() %>% 
      group_by(uniquecarrier) %>% 
      summarize(average_delay = mean(arrdelay)) %>% 
      arrange(-average_delay)
    
    avg_carrier_delay_duration <- head(avg_carrier_delay_duration, 10)
    
    melt_acdd= melt(avg_carrier_delay_duration[,c("uniquecarrier", "average_delay")], id.vars = 1)
    
    ggplot(melt_acdd, aes(x=uniquecarrier, y=value))+
      geom_bar(aes(fill= variable), stat="identity", position="dodge") +
      labs(x= "Carrier",
           y = "Duration of Delay",
           title = paste("Average Delay Duration -", input$carrier),
           fill = "Duration") +
      theme_wsj()+
      theme(axis.text = element_text(size = 9),
            plot.title = element_text(size=14, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 13, face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 11, face = "bold"),
            legend.position = "right") +
      coord_flip()
    
    })     
    
    
    
  }
    
    
    

    
    })
  
  
})