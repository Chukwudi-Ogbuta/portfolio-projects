library(shiny)
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(rio)
library(ggthemes)
library(fontawesome)
library(shinydashboard)

shinyServer(function(input,output){
  
  # Import data set
  sa_apartment <- import("south_africa_apartments.csv")
  
  
  # Display name
  
  showNotification("Created by Chukwudi Ogbuta", duration = NULL, type = "message")
  
  # Observe event for city selection
  
  observeEvent(input$city_list, {if (input$city_list == "All"){
                                                                
        # Design value outbox for average rent 
                                                                        
        output$avg_rent <- renderValueBox({
                                            average_rent <- as.integer(mean(sa_apartment$RENT))
                                            valueBox("Average Rent", value = tags$p(average_rent, style= "font-size: 70%;"),color = "teal")
                                           })
                                                                
         # Design value outbox for minimum rent 
                                                                
         output$min_rent <- renderValueBox({
                                            minimum_rent <- as.integer(min(sa_apartment$RENT))
                                            valueBox("Minimum Rent", value = tags$p(minimum_rent, style= "font-size: 70%;"),color = "teal")
                                           })
                                                                
                                                                
         # Design value outbox for maximum rent 
                                                                
         output$max_rent <- renderValueBox({
                                            maximum_rent <- as.integer(max(sa_apartment$RENT))
                                            valueBox("Maximum Rent", value = tags$p(maximum_rent, style= "font-size: 70%;"),color = "teal")
                                           })
                                                         
                                                                
         # Plot a histogram that takes all cities' rent into consideration
                                                                
         output$hist_plot <- renderPlot({
                                           sa_apartment %>%
                                               ggplot(aes(x=RENT)) +
                                               geom_histogram(color= "black", fill="turquoise3")+
                                               labs( title = "Distribution of Rent in 5 S.A Cities",
                                                     subtitle = "Dataset from Property.co.mg.za",
                                                     x = "Rent",
                                                     y = "Number of Houses")+
                                                theme_clean() + scale_colour_ptol() +
                                                theme(axis.text = element_text(angle = 90, color = "black",size = 12),
                                                      plot.title = element_text(face = "bold", color = "black"),
                                                      plot.subtitle = element_text(color="black", size = 10),
                                                      axis.title = element_text(color = "black", size = 14 ),
                                                      plot.background = element_rect(fill = "white")) })
                                                                
         # Create box plot that shows relationship between rent and apartment size in all cities
                                                                
         output$box_plot <- renderPlot({
                                            sa_apartment %>%
                                                ggplot(aes(x=APARTMENT_SIZE, y=RENT))+
                                                geom_boxplot(color="black", fill="turquoise3")+
                                                coord_flip()+
                                                labs( title = "Distribution of Rent Based On Apartment Type",
                                                      subtitle = "Dataset from Property.co.mg.za",
                                                      y = "Rent",
                                                      x = "Apartment Size")+
                                                theme_clean() + scale_colour_ptol() +
                                                theme(axis.text.x = element_text(angle = 90, color = "black", size = 12),
                                                      axis.text.y = element_text(color = "black", size = 12),
                                                      plot.title = element_text(face = "bold", color = "black"),
                                                      plot.subtitle = element_text(color="black", size = 10),
                                                      axis.title = element_text(color = "black", size = 14 ),
                                                      plot.background = element_rect(fill = "white"))})
                                                                
        # Table output for top 10 cheapest accommodation and most expensive
                                                                
        output$table_cheap <- renderPlot({
                                            top_data = sa_apartment %>%
                                               arrange(RENT)
                                            
                                            top_data = head(top_data, 10)
                                            
                                            ggplot(top_data,aes(x=LOCATION, fill=APARTMENT_SIZE))+
                                            geom_bar()+
                                            labs(title = "Top 10 Most Affordable Locations",
                                                 fill = "APARTMENT SIZE",
                                                 x = "Locations",
                                                 y = "Number of Houses")+
                                             coord_flip()+
                                             theme_clean() + scale_colour_ptol() +
                                             theme(axis.text.x = element_text(angle = 90, color = "black", size = 12),
                                                   axis.text.y = element_text(color = "black", size = 12),
                                                   plot.title = element_text(face = "bold", color = "black"),
                                                   axis.title = element_text(color = "black", size = 14 )) 
                                                                                                        
                                                                                                        
                                            })
                                                                
        output$table_expensive <- renderPlot({
                                               top_data = sa_apartment %>%
                                                   arrange(RENT)
                                                                                                            
                                               top_data = tail(top_data, 10)
                                                                                                            
                                               ggplot(top_data,aes(x=LOCATION, fill=APARTMENT_SIZE))+
                                               geom_bar()+
                                               labs(title = "Top 10 Most Expensive Locations",
                                                    fill = "APARTMENT SIZE",
                                                    x = "Locations",
                                                    y = "Number of Houses")+
                                               coord_flip()+
                                               theme_clean() + scale_colour_ptol() +
                                               theme(axis.text.x = element_text(angle = 90, color = "black", size = 12),
                                                     axis.text.y = element_text(color = "black", size = 12),
                                                     plot.title = element_text(face = "bold", color = "black"),
                                                     axis.title = element_text(color = "black", size = 14 )) 
                                                 })
                                                                
                                                                
} else { 
         # Filter original data to contain just selected city
                                                           
         sa_apartment_edit <- reactive({sa_apartment %>% filter(CITY_NAME == input$city_list)})
                                                                    
                                                                    
         # Design infobox for average rent 
                                                                    
         output$avg_rent <- renderValueBox({
                                             average_rent <- as.integer(mean(sa_apartment_edit()$RENT))
                                             valueBox("Average Rent", value = tags$p(average_rent, style= "font-size: 70%;"),color = "teal")
                                            })
                                                                    
         # Design infobox for minimum rent 
                                                                    
         output$min_rent <- renderValueBox({
                                             minimum_rent <- as.integer(min(sa_apartment_edit()$RENT))
                                             valueBox("Minimum Rent", value = tags$p(minimum_rent, style= "font-size: 70%;"),color = "teal")
                                             })
                                                                    
                                                                    
         # Design infobox for maximum rent 
                                                                    
         output$max_rent <- renderValueBox({
                                            maximum_rent <- as.integer(max(sa_apartment_edit()$RENT))
                                            valueBox("Maximum Rent", value = tags$p(maximum_rent, style= "font-size: 70%;"),color = "teal" )
                                            })
                                                                    
                                                                    
         # Plot a histogram that takes selected city's rent into consideration
         
         output$hist_plot <- renderPlot({
                                          ggplot(data = sa_apartment_edit(), aes(x=RENT)) +
                                              geom_histogram(color= "black", fill="turquoise3")+
                                              labs( title = paste("Distribution of Rent in", input$city_list ),
                                                    subtitle = "Dataset from Property.co.mg.za",
                                                    x = paste("Rent in", input$city_list),
                                                    y = paste("Number of Houses in", input$city_list))+
                                               theme_clean() + scale_colour_ptol() +
                                               theme(axis.text = element_text(angle = 90, color = "black", size = 12),
                                                     plot.title = element_text(face = "bold", color = "black"),
                                                     plot.subtitle = element_text(color="black", size = 10),
                                                     axis.title = element_text(color = "black", size = 14 ),
                                                     plot.background = element_rect(fill = "white"))
                                            })
                                                                    
          # Create box plot that shows relationship between rent and apartment size in selected city
                                                                    
          output$box_plot <- renderPlot({
                                          ggplot(data = sa_apartment_edit(), aes(x=APARTMENT_SIZE, y=RENT))+
                                               geom_boxplot(color="black", fill="turquoise3")+
                                               coord_flip()+
                                               labs( title = paste("Distribution of Rent Based On Apartment Type in", input$city_list),
                                                     subtitle = "Dataset from Property.co.mg.za",
                                                     y = paste("Rent in", input$city_list),
                                                     x = "Apartment Size")+
                                                theme_clean() + scale_colour_ptol() +
                                                theme(axis.text.x = element_text(angle = 90, color = "black", size = 12),
                                                      axis.text.y = element_text(color = "black", size = 12),
                                                      plot.title = element_text(face = "bold", color = "black"),
                                                      plot.subtitle = element_text(color="black", size = 10),
                                                      axis.title = element_text(color = "black", size = 14 ),
                                                      plot.background = element_rect(fill = "white"))})
                                                                    
                                                                    
           # Bar chart output for top 10 cheapest accommodation and most expensive
                                                                    
          output$table_cheap <- renderPlot({
                                             top_data = sa_apartment_edit() %>%
                                                   arrange(RENT)
                                              
                                             top_data = head(top_data, 10)
                                              
                                              ggplot(top_data,aes(x=LOCATION, fill=APARTMENT_SIZE))+
                                                  geom_bar()+
                                                  labs(title = "Top 10 Most Affordable Locations",
                                                       fill = "APARTMENT SIZE",
                                                       x = "Locations",
                                                       y = "Number of Houses")+
                                                  coord_flip()+
                                                  theme_clean() + scale_colour_ptol() +
                                                  theme(axis.text.x = element_text(angle = 90, color = "black", size = 12),
                                                        axis.text.y = element_text(color = "black", size = 12),
                                                        plot.title = element_text(face = "bold", color = "black"),
                                                        axis.title = element_text(color = "black", size = 14 )) 
                                                                      
                                                                      
                                             })
                                                                    
          output$table_expensive <- renderPlot({
                                                  top_data = sa_apartment_edit() %>%
                                                        arrange(RENT)
                                                  
                                                  top_data = tail(top_data, 10)
                                                                      
                                                   ggplot(top_data,aes(x=LOCATION, fill=APARTMENT_SIZE))+
                                                     geom_bar()+
                                                     labs(title = "Top 10 Most Expensive Locations",
                                                          fill = "APARTMENT SIZE",
                                                          x = "Locations",
                                                          y = "Number of Houses")+
                                                     coord_flip()+
                                                     theme_clean() + scale_colour_ptol() +
                                                     theme(axis.text.x = element_text(angle = 90, color = "black", size = 12),
                                                           axis.text.y = element_text(color = "black", size = 12),
                                                           plot.title = element_text(face = "bold", color = "black"),
                                                           axis.title = element_text(color = "black", size = 14 )) 
                                                  })
                                                                    
        }
    
    
    
    })
  
})


