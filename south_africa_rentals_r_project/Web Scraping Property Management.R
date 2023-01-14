# Project: Scraping and analyzing cost of houses in 5 South African Cities

library(robotstxt)                                                # For checking website scraping permission
library(rvest)                                                    # For scraping the data                                              
library(XML)                                                      # For scraping the data
library(tidyverse)                                                # For data cleaning and transformation
library(stringr)                                                  # For data cleaning and transformation
library(ggplot2)                                                  # For data visualization
library(plotly)                                                   # For data visualization
library(ggthemes)                                                 # For data visualization


# Check for web scraping permission

paths_allowed("https://property.mg.co.za/houses-to-rent-in-johannesburg-c100?Page=2")

# Create objects to hold url

capetown_url <- "https://property.mg.co.za/houses-to-rent-in-cape-town-c432?Page="
pretoria_url <- "https://property.mg.co.za/houses-to-rent-in-pretoria-c1?Page="
johannesburg_url <- "https://property.mg.co.za/houses-to-rent-in-johannesburg-c100?Page="
soweto_url <- "https://property.mg.co.za/houses-to-rent-in-soweto-c102?Page=" 
eastlondon_url <- "https://property.mg.co.za/houses-to-rent-in-east-london-c216?Page="

# Create empty vectors to contain scraped data

capetown_data <- vector()
pretoria_data  <- vector()
johannesburg_data <- vector()
soweto_data <- vector()
eastlondon_data <- vector()

# Begin scraping operation by using a for loop to iterate over pages

for (i in seq(from=1, to=10)) 
{
  
  capetown_url <- paste0(capetown_url,i)
  
  capetown_data <- c(capetown_data, read_html(capetown_url) %>%
                       html_nodes(".sc_listingTileContent") %>%
                       html_text())


  pretoria_url <- paste0(pretoria_url,i)
  
  pretoria_data <- c(pretoria_data, read_html(pretoria_url) %>%
                       html_nodes(".sc_listingTileContent") %>%
                       html_text())


  johannesburg_url <- paste0(johannesburg_url,i)
  
  johannesburg_data <- c(johannesburg_data, read_html(johannesburg_url) %>%
                       html_nodes(".sc_listingTileContent") %>%
                       html_text())
  

  soweto_url <- paste0(soweto_url,i)
  
  soweto_data <- c(soweto_data, read_html(soweto_url) %>%
                           html_nodes(".sc_listingTileContent") %>%
                           html_text())
  
  
  eastlondon_url <- paste0(eastlondon_url,i)
  
  eastlondon_data <- c(eastlondon_data, read_html(eastlondon_url) %>%
                     html_nodes(".sc_listingTileContent") %>%
                     html_text())
  
    
    
  print(i)  
}

# Create a vector to combine all of the results

sa_property_data <- vector()

sa_property_data <- c( capetown_data,
                      pretoria_data,
                      johannesburg_data,
                      soweto_data,
                      eastlondon_data
                    )


# Create a column for city name

CITY_NAME <- character()


for (i in 1:420)
{
  CITY_NAME[i] <- case_when( i<=84 ~ "Cape Town",
                             i > 84  & i <= 168 ~ "Pretoria",
                             i > 168 & i <= 252 ~ "Johannesburg",
                             i > 252 & i <= 336 ~ "Soweto",
                             i > 336 & i <= 420 ~ "East London")
}


# ----------------------- Cleaning scraped data to extract columns -------------------------------------


# Replacing all \r\n in data using gsub

sa_property_data <- gsub("\r\n", "", sa_property_data)


# Trim white spaces

sa_property_data <- str_trim(sa_property_data, "both")


# Extract Monthly Rent (First 2 values in string)

sa_text_split <- str_split(sa_property_data, " ")


second_value <- character()
third_value <- character()

split_rent <- data.frame(second_value,third_value)
sa_rent <- data.frame(gather_rent=character())

for (i in 1:420)
{
  split_rent[i,] = sa_text_split[[i]][2:3]
}

sa_rent <- paste0(split_rent$second_value,split_rent$third_value,sep = " ")

RENT <- vector()

RENT <- as.numeric(sa_rent)


# Extracting Apartment size (first 3 values in reduced string)


sa_property_data_2 <- str_sub(sa_property_data,80, nchar(sa_property_data))

sa_property_data_2 <- str_trim(sa_property_data_2, "left")


sa_text_split_2 <- str_split(sa_property_data_2, " ")

split_apartment <- data.frame(first_value = character(), second_value= character(), third_value= character())

sa_apartment <- data.frame(gather_apartment=character())


for (i in 1:420) 
{
  split_apartment[i,] = sa_text_split_2[[i]][1:3]  
}

sa_apartment <- paste(split_apartment$first_value,split_apartment$second_value, split_apartment$third_value, sep = " ")

APARTMENT_SIZE <- character()

APARTMENT_SIZE <- sa_apartment


# Extracting Location using regexpr

matches <- regexpr("in.+", sa_property_data_2)
location_string <-  regmatches(sa_property_data_2, matches)


sa_text_split_3 <- str_split(location_string, " ")


split_location <- data.frame(first_value = character(), 
                             second_value = character(), 
                             third_value = character(),
                             fourth_value = character(),
                             fifth_value = character())

sa_location <- data.frame(gather_location <- character())

for (i in 1:420) 
{
  split_location[i,] <- sa_text_split_3[[i]][1:5]    
}

sa_location <- paste(split_location$first_value,
                     split_location$second_value,
                     split_location$third_value,
                     split_location$fourth_value,
                     split_location$fifth_value, sep = " ")

LOCATION <- character()

LOCATION <- str_trim(gsub("in", "", sa_location), "both")


# Combining variables together to create a data frame

property_df <- data.frame(cbind(CITY_NAME, RENT, APARTMENT_SIZE, LOCATION))



# ----------------------------- Handling wrong data type and dealing with missing Values ----------------------------

# Note: If you scrape this data at a later time, the errors may occur in a different row, always verify the position of the errors while cleaning

# Convert rent to a numeric quantity

property_df$RENT <- as.numeric(property_df$RENT)

# Checking for null values: found 6 in rent

summary(property_df)


which(is.na(property_df$RENT))

# Replacing null values with the average value of rent in cape Town and pretoria with average values of apartments with same size


# -----------------3 bedroom in capetown null values replaced 


bedroom_capetown_3 <- property_df %>%
  filter(CITY_NAME == 'Cape Town' & APARTMENT_SIZE == '3 Bedroom House')

bedroom_capetown_3 %>%
  summarize(average_rent = mean(RENT, na.rm = TRUE))


 property_df$RENT[25] = 19460


# -----------------6 bedroom in capetown null values replaced

bedroom_capetown_6 <- property_df %>%
  filter(CITY_NAME == 'Cape Town' & APARTMENT_SIZE == '6 Bedroom House') 


bedroom_capetown_6 %>%
  summarize(average_rent = mean(RENT, na.rm = TRUE))

property_df$RENT[27] = 92900
property_df$RENT[31] = 92900


# ----------------------- 5 bedroom in pretoria null values replaced

bedroom_pretoria_5 <- property_df %>%
  filter(CITY_NAME == 'Pretoria' & APARTMENT_SIZE == '5 Bedroom House') 


bedroom_pretoria_5 %>%
  summarize(average_rent = mean(RENT, na.rm = TRUE))


property_df$RENT[86] = 38666
property_df$RENT[128] = 38666
property_df$RENT[149] = 38666

which(is.na(property_df$RENT))


# Checking unique values in each variable to scan for incorrect data


unique(property_df$CITY_NAME)                                                   # No incorrect data found

unique(property_df$APARTMENT_SIZE)                                              # Incorrect data found

property_df <- property_df %>%
  filter(!APARTMENT_SIZE == 'House to rent')                                    # Incorrect data fixed


unique(property_df$LOCATION)                                                    # No incorrect data found                               


# Remove duplicates

property_df <- unique(property_df)

# Writing to csv file

write.csv(property_df, file = "south_africa_apartments.csv", row.names = FALSE)


# Read file back into R

sa_apartment <-  read.csv("south_africa_apartments.csv", header = TRUE, sep = ",")

# Data Visualization

# distribution of rent in 5 cities - Histogram

sa_apartment %>%
  ggplot(aes(x=RENT)) +
  geom_histogram(color= "black", fill="gold", aes(y = stat(density)))+
  geom_density(color="red4")+
  labs( title = "Distribution of Rent in 5 S.A Cities",
        subtitle = "Dataset from Property.co.mg.za",
        x = "Rent",
        y = "Number of Houses")+
  theme_excel_new() + scale_colour_ptol() +
  theme(axis.text = element_text(angle = 90, color = "goldenrod"),
        plot.title = element_text(face = "bold", color = "red4"),
        plot.subtitle = element_text(color="goldenrod", size = 10),
        axis.title = element_text(color = "red4" ))

# Relationship between rent and Apartment

sa_apartment %>%
  ggplot(aes(x=APARTMENT_SIZE, y=RENT))+
  geom_boxplot(color="black", fill="gold")+
  coord_flip()+
  labs( title = "Distribution of Rent Based On Apartment Type",
        subtitle = "Dataset from Property.co.mg.za",
        x = "Rent",
        y = "Apartment Size")+
  theme_excel_new() + scale_colour_ptol() +
  theme(axis.text.x = element_text(angle = 90, color = "goldenrod"),
        axis.text.y = element_text(color = "goldenrod"),
        plot.title = element_text(face = "bold", color = "red4"),
        plot.subtitle = element_text(color="goldenrod", size = 10),
        axis.title = element_text(color = "red4" ))


