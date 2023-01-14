library(tidyverse)                                                # For data cleaning and manipulation
library(mgsub)                                                    # For multiple string substitution
library(RODBC)                                                    # For connection to Microsoft SQL server

# Load data set

mental_health_df <-  read_csv("Mental health Depression disorder Data.csv")

# Standardize column headers

names(mental_health_df) <- gsub("\\(.*", "", names(mental_health_df))

names(mental_health_df)  <- str_trim(names(mental_health_df),"both")

mental_health_df <- mental_health_df %>% 
  rename(Country = Entity)

# Check for null values

anyNA(mental_health_df)

which(is.na(mental_health_df))

# Scanning through nature of null values

mental_health_df[217219:217224,]

# Dropping null values

mental_health_df <- mental_health_df %>% 
  drop_na()

# Re-check for null values

anyNA(mental_health_df)

# Checking data type 

glimpse(mental_health_df)

# Checking duplicated data

sum(duplicated(mental_health_df))

# Reshape data from wide data to long data

mental_health_df <- mental_health_df %>% 
  gather(mental_disorder,percentage_of_cases, Schizophrenia:'Alcohol use disorders')

names(mental_health_df) <- tolower(names(mental_health_df))

# Creating connection to database

conn <- odbcConnect("rsample", rows_at_time=1)

# Uploading data to Microsoft SQL server

sqlSave(conn,mental_health_df,tablename = "mental_health", append = T, rownames = F)

# Querying first 10 row of data

sqlQuery(conn, "SELECT TOP 10 * 
                FROM mental_health")

# List of countries in dataset

sqlQuery(conn, "SELECT DISTINCT country
                FROM mental_health
                ORDER BY country")

# List of years in dataset

sqlQuery(conn, "SELECT DISTINCT year
                FROM mental_health
                ORDER BY year")

# Change name of Column

sqlQuery(conn, "sp_rename 'mental_health.percentage_of_cases', 'percentage_of_population'")

#--------------------------- ANNUAL OVERVIEW -------------------------------------------------

# Total Mental disorder cases each year by country

sqlQuery(conn, "SELECT country,
                year,
                SUM(percentage_of_population) AS 'population_mental_disorder(%)'
                FROM mental_health
                GROUP BY country,year
                ORDER BY country,year")

# Query to see top 10 years with highest reported cases (% population) from each country

sqlQuery(conn, "SELECT TOP 10
                country,
                year,
                SUM(percentage_of_population) AS 'population_mental_disorder(%)'
                FROM mental_health
                GROUP BY country,year
                ORDER BY SUM(percentage_of_population) DESC")

# Query to see top 10 years with least reported cases (% population) from each country

sqlQuery(conn, "SELECT TOP 10
                country,
                year,
                SUM(percentage_of_population) AS 'population_mental_disorder(%)'
                FROM mental_health
                GROUP BY country,year
                ORDER BY SUM(percentage_of_population)")

# Query to compare cases recorded each year with the recorded cases for the preceding year

sqlQuery(conn, "SELECT country,
                year,
                SUM(percentage_of_population) AS 'population_mental_disorder(%)',
                LAG(SUM(percentage_of_population)) OVER (PARTITION BY country ORDER BY country, year) AS 'yearly_comparison',
                CASE WHEN 
                  LAG(SUM(percentage_of_population)) OVER (PARTITION BY country ORDER BY country, year)> SUM(percentage_of_population) THEN 'Reduced'
                WHEN
                  LAG(SUM(percentage_of_population)) OVER (PARTITION BY country ORDER BY country, year)< SUM(percentage_of_population) THEN 'Increased'
                END AS 'comparison_status'
                FROM mental_health
                GROUP BY country,year")

# Query to see country with highest percentage of recorded cases 

sqlQuery(conn, "WITH yearly_cases AS
                (SELECT country,
                year,
                SUM(percentage_of_population) AS 'population_mental_disorder'
                FROM mental_health
                GROUP BY country,year)
         
                SELECT country, 
                year,
                population_mental_disorder
                FROM yearly_cases
                WHERE population_mental_disorder IN
                (SELECT MAX(population_mental_disorder) FROM yearly_cases)")


#-----------------------BREAKDOWN BY MENTAL DISORDER--------------------------------------


# Maximum (highest percentage of population recorded) cases based on type of mental disorder


# Maximum - Schizophrenia

sqlQuery(conn, "SELECT country,
                year,
                mental_disorder,
                percentage_of_population 
                FROM mental_health
                WHERE percentage_of_population IN
                (SELECT MAX (percentage_of_population) 
                FROM mental_health 
                WHERE mental_disorder='Schizophrenia')")


# Maximum - Bipolar disorder

sqlQuery(conn, "SELECT country,
                year,
                mental_disorder,
                percentage_of_population 
                FROM mental_health
                WHERE percentage_of_population IN
                (SELECT MAX (percentage_of_population) 
                FROM mental_health 
                WHERE mental_disorder='Bipolar disorder')")

# Maximum - Eating disorders

sqlQuery(conn, "SELECT country,
                year,
                mental_disorder,
                percentage_of_population 
                FROM mental_health
                WHERE percentage_of_population IN
                (SELECT MAX (percentage_of_population) 
                FROM mental_health 
                WHERE mental_disorder='Eating disorders')")

# Maximum - Anxiety disorders

sqlQuery(conn, "SELECT country,
                year,
                mental_disorder,
                percentage_of_population 
                FROM mental_health
                WHERE percentage_of_population IN
                (SELECT MAX (percentage_of_population) 
                FROM mental_health 
                WHERE mental_disorder='Anxiety disorders')")

# Maximum - Drug use disorders

sqlQuery(conn, "SELECT country,
                year,
                mental_disorder,
                percentage_of_population 
                FROM mental_health
                WHERE percentage_of_population IN
                (SELECT MAX (percentage_of_population) 
                FROM mental_health 
                WHERE mental_disorder='Drug use disorders')")

# Maximum - Depression

sqlQuery(conn, "SELECT country,
                year,
                mental_disorder,
                percentage_of_population 
                FROM mental_health
                WHERE percentage_of_population IN
                (SELECT MAX (percentage_of_population) 
                FROM mental_health 
                WHERE mental_disorder='Depression')")

# Maximum - Alcohol use disorders

sqlQuery(conn, "SELECT country,
                year,
                mental_disorder,
                percentage_of_population 
                FROM mental_health
                WHERE percentage_of_population IN
                (SELECT MAX (percentage_of_population) 
                FROM mental_health 
                WHERE mental_disorder='Alcohol use disorders')")


odbcClose(conn)













