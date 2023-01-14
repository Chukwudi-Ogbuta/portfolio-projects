# Load libraries

library(tidyverse)                                                      # For data manipulation
library(ggthemes)                                                       # For data visualization
library(reshape2)                                                       # For melting data frame
library(tidymodels)                                                     # For building models
library(ggplot2)                                                        # For data visualization

# ------------------------------------------------------------------------------

# Load csv file into data frame

flight_delay <- read.csv("DelayedFlights.csv", header = TRUE, sep = ",")


# ------------------------------------------------------------------------------

# Questions

# Is there a specific carrier that experiences delay the most?
# Are there specific days of the week where flight delay occurs the most
# what was the total number of flights by each airline and what percentage was delayed?
# What are the key factors that cause delay in flights? (Test Depdelay, Carrier Delay, Weather Delay, NAS Delay, Security Delay, LateAircraft Delay)


# ------------------------------------------------------------------------------

# Drop Columns not relevant to this analysis

flight_df <-  flight_delay %>%
  select(Year, 
         Month, 
         DayofMonth, 
         DayOfWeek, 
         UniqueCarrier, 
         ArrDelay, 
         DepDelay, 
         Origin, 
         Dest,
         CarrierDelay,
         WeatherDelay,
         NASDelay,
         SecurityDelay,
         LateAircraftDelay)


# Remove all null values in Arr Delay column

null_dropped <- flight_df %>%
  drop_na(ArrDelay)

# Replace null values in carrier, weather, NAS, security and Late aircraft delay


null_replaced <- null_dropped %>%
  replace_na(list(CarrierDelay = 0,
                  WeatherDelay = 0,
                  NASDelay = 0,
                  SecurityDelay = 0,
                  LateAircraftDelay = 0))


# Convert day of week to weekday name

weekday_converted <- null_replaced %>%
  mutate(WeekDay = case_when(DayOfWeek == 1 ~ "Mon",
                             DayOfWeek == 2 ~ "Tue",
                             DayOfWeek == 3 ~ "Wed",
                             DayOfWeek == 4 ~ "Thu",
                             DayOfWeek == 5 ~ "Fri",
                             DayOfWeek == 6 ~ "Sat",
                             DayOfWeek == 7 ~ "Sun"))

# Change the position of this new column in the data frame

weekday_converted <- weekday_converted %>%
  relocate(WeekDay, .after = DayOfWeek)

# Remove DayOfWeek column

weekday_converted <- weekday_converted %>%
  select(-DayOfWeek)


# Concatenate Origin and Dest to create route column

route_df <- weekday_converted %>%
  mutate(Route = paste(Origin, "to", Dest))
  
route_df <- route_df %>%
  relocate(Route, .after = Dest)

# ------------------------------------------------------------------------------

# Save data frame to csv file

write.csv(route_df, file = "clean_flight_delay.csv", row.names = FALSE)

# ------------------------------------------------------------------------------

# Load data for Exploratory Analysis

new_flight_df <- read_csv("clean_flight_delay.csv")

# ------------------------------------------------------------------------------ Further Cleaning

# Standardize column header to lower case

new_flight_df <- new_flight_df %>%
  rename_with(tolower)

# Check data type of variables

glimpse(new_flight_df)


# Converting month numbers to month name

new_flight_df <- new_flight_df %>% 
  mutate(month_name = case_when(month == 1 ~ "Jan",
                                month == 2 ~ "Feb",
                                month == 3 ~ "Mar",
                                month == 4 ~ "Apr",
                                month == 5 ~ "May",
                                month == 6 ~ "Jun",
                                month == 7 ~ "Jul",
                                month == 8 ~ "Aug",
                                month == 9 ~ "Sep",
                                month == 10 ~ "Oct",
                                month == 11 ~ "Nov",
                                month == 12 ~ "Dec"))

new_flight_df <- new_flight_df %>%
  relocate(month_name, .after = month)

# Dropping numeric month numbering

new_flight_df <- new_flight_df %>% 
  select(-month)

# Removing all negative values of arrdelay

new_flight_df$arrdelay[new_flight_df$arrdelay<0] = 0

# Save to csv file again

write.csv(new_flight_df, file = "clean_flight_delay.csv", row.names = FALSE)

# ------------------------------------------------------------------------------

# Check number of flights by carriers throughout the year

sort(table(new_flight_df$uniquecarrier), decreasing = T)

new_flight_df %>% 
  nrow()

# Key note: Carrier WN had the most flights throughout the year (376201) and AQ had the least number of flights (744)



# ------------------------------------------------------------------------------

# Average delay duration through out the year by carriers

new_flight_df %>% 
  group_by(uniquecarrier) %>% 
  summarize(average_delay = mean(arrdelay)) %>% 
  arrange(-average_delay)



# Key notes: AQ had the least average delay, while B6 had  the highest average delay throughout the year


# ------------------------------------------------------------------------------

# Average monthly delay by carriers


carrier_monthly_delays <- new_flight_df %>% 
  group_by(uniquecarrier, month_name) %>% 
  summarise(avg_monthly_delay = mean(arrdelay)) %>% 
  arrange(avg_monthly_delay)

# Data viz: average monthly delays by carriers
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
  theme(axis.text = element_text(size = 7),
        plot.title = element_text(size=13, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.position = "bottom")

# ------------------------------------------------------------------------------

# Average delay by carriers on weekdays 

carrier_weekly_delay <- new_flight_df %>% 
  group_by(uniquecarrier, weekday) %>% 
  summarise(avg_weekly_delay = mean(arrdelay)) %>% 
  arrange(avg_weekly_delay)


# Data Viz: average weekly delays by carrier

carrier_weekly_delay$weekday <- as.factor(carrier_weekly_delay$weekday)
levels(carrier_weekly_delay$weekday) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

carrier_weekly_delay %>% 
  ggplot(aes(x=uniquecarrier, y= weekday, fill=avg_weekly_delay))+
  geom_tile(color= "white", size=0.15) +
  scale_fill_gradient2(low = "white", high = "blue")+
  labs(x= "Unique Carrier",
       y = "Week Days",
       title = "Average Delay by Carriers on Weekdays",
       fill = "Average Delay") +
  theme_wsj()+
  theme(axis.text = element_text(size = 7),
        plot.title = element_text(size=13, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.position = "bottom")

# ------------------------------------------------------------------------------

#  Total Flights by each Carrier and % of flight that was delayed

carrier_flight <- new_flight_df %>% 
  count(uniquecarrier) 

carrier_delay <- new_flight_df %>% 
  filter(arrdelay>0) %>% 
  count(uniquecarrier)

flight_delay <- data.frame(carrier_flight, carrier_delay[,2])

flight_delay <- flight_delay %>% 
  rename(total_flight = n, 
         total_delayed = n.1)

carrier_flight_delay <-  flight_delay %>% 
  mutate(percentage_delay = as.integer((total_delayed/total_flight)*100),
         percentage_on_time = 100 - as.integer((total_delayed/total_flight)*100))


# Data Viz: percentage of flight delayed by unique carrier

dfm= melt(carrier_flight_delay[,c("uniquecarrier", "percentage_delay", "percentage_on_time")], id.vars = 1)

ggplot(dfm, aes(x=uniquecarrier, y=value))+
  geom_bar(aes(fill= variable), stat="identity", position="dodge") +
  labs(x= "Unique Carrier",
       y = "Value",
       title = "Percentage of Delayed Flights vs On Time Flights") +
  theme_wsj()+
  theme(axis.text = element_text(size = 7),
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.position = "bottom")

# ------------------------------------------------------------------------------

# Checking to see arrival delay by route (Top 10 routes with the most delay)

top_10_delayed_route <- new_flight_df %>% 
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
  theme(axis.text = element_text(size = 7),
        plot.title = element_text(size=12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.position = "right") +
  coord_flip()


# ------------------------------------------------------------------------------

# Normalize key variables (using min-max method) to check correlation and build models

new_flight_normalized <- new_flight_df %>% 
  mutate(arrdelay = (arrdelay - min(arrdelay))/max(arrdelay) - min(arrdelay),
         depdelay = (depdelay - min(depdelay))/max(depdelay) - min(depdelay),
         nasdelay = (nasdelay - min(nasdelay))/max(nasdelay) - min(nasdelay),
         carrierdelay = (carrierdelay - min(carrierdelay))/max(carrierdelay) - min(carrierdelay),
         lateaircraftdelay = (lateaircraftdelay - min(lateaircraftdelay))/max(lateaircraftdelay) - min(lateaircraftdelay))

write.csv(new_flight_normalized, file = "new_flight_normalized.csv", row.names = F)

# Checking correlation between arrdelay and depdelay (0.955)

new_flight_normalized %>% 
  select(arrdelay,depdelay) %>% 
  cor(method = "pearson")

# Checking correlation between arrdelay and carrierdelay (0.546)

new_flight_normalized %>% 
  select(arrdelay,carrierdelay) %>% 
  cor(method = "pearson")

# Checking correlation between arrdelay and weatherdelay (0.279)

new_flight_normalized %>% 
  select(arrdelay,weatherdelay) %>% 
  cor(method = "pearson")

# Checking correlation between arrdelay and nasdelay (0.45)

new_flight_normalized %>% 
  select(arrdelay,nasdelay) %>% 
  cor(method = "pearson")

# Checking correlation between arrdelay and securitydelay (0.01)

new_flight_normalized %>% 
  select(arrdelay,securitydelay) %>% 
  cor(method = "pearson")

# Checking correlation between arrdelay and lateaircraftdelay (0.557)

new_flight_normalized %>% 
  select(arrdelay,lateaircraftdelay) %>% 
  cor(method = "pearson")

# ------------------------------------------------------------------------------

# Building regression models to predict arrival delay

set.seed(1998)
flight_split <- initial_split(new_flight_normalized, prop = 0.75)
train_data <- training(flight_split)
test_data <- testing(flight_split)

# Create linear regression engine

lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm") 

# Model 1: Simple Linear Regression Model

lm_model <- lm_spec %>% 
  fit(arrdelay ~ depdelay + 
                 carrierdelay + 
                 nasdelay + 
                 lateaircraftdelay,
      data = train_data)

lm_test_result <- lm_model %>% 
  predict(new_data = test_data) %>% 
  mutate(truth = test_data$arrdelay)

lm_rmse <- rmse(lm_test_result, truth = truth, estimate = .pred)
lm_rsq <- rsq(lm_test_result, truth = truth, estimate = .pred)


# Model 2: Polynomial Regression Model

poly_model <- lm_spec %>% 
  fit(arrdelay ~ poly(depdelay, 7) +
                 poly(carrierdelay, 5) +
                 poly(nasdelay, 4) +
                 poly(lateaircraftdelay, 5),
      data = train_data)

poly_test_result <- poly_model %>% 
  predict(new_data = test_data) %>% 
  mutate(truth = test_data$arrdelay)

poly_rmse <- rmse(poly_test_result, truth = truth, estimate = .pred)
poly_rsq <- rsq(poly_test_result, truth = truth, estimate = .pred)

# Model 3: Ridge regularization model

flight_recipe <- 
  recipe(arrdelay ~ depdelay + 
                    carrierdelay +
                    nasdelay +
                    lateaircraftdelay, 
         data = train_data)

# Set engine 

ridge_spec <- linear_reg(penalty = 0.001, mixture=0) %>%
  set_engine("glmnet")

ridge_wf <- workflow() %>%
  add_recipe(flight_recipe)

ridge_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)

ridge_test_result <- ridge_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth=test_data$arrdelay)

ridge_rmse <-  rmse(ridge_test_result, truth=truth, estimate = .pred)
ridge_rsq <- rsq(ridge_test_result, truth=truth, estimate = .pred)

# Creating dataframe of models, rmse and rsq

model_name <- c("linear", "polynomial", "ridge")
rmse_values <- c(lm_rmse$.estimate, poly_rmse$.estimate, ridge_rmse$.estimate)
rsq_values <- c(lm_rsq$.estimate, poly_rsq$.estimate, ridge_rsq$.estimate)

model_df <- data.frame(model_name, rmse_values, rsq_values)

# Data viz of results from models

model_viz = melt(model_df[,c("model_name", "rmse_values", "rsq_values")], id.vars = 1)

ggplot(model_viz, aes(x=model_name, y=value))+
  geom_bar(aes(fill= variable), stat="identity", position="dodge")

# Q-Q plot of the best model (polynomial model)

# Note: my Rstudio keeps on crashing when i run this, i believe it is because of the size of the data (over 1 million rows)

poly_test_result %>%
  ggplot(aes(x=truth, y=.pred))+
  geom_point(alpha=0.4, color= "red")+
  geom_smooth(method = "lm", color="slateblue2")
