library(tidyverse)                                  # For data cleaning and manipulation
library(RODBC)                                      # For connection to SQL database
library(mgsub)                                      # For string manipulation

lung_cancer_df <- read_csv("survey lung cancer.csv")


# Convert all column headers to lowercase and replace white space with underscore

names(lung_cancer_df) <- tolower(names(lung_cancer_df))

names(lung_cancer_df) <- gsub(" ", "_", names(lung_cancer_df))

# Checking for null values

anyNA(lung_cancer_df)

# Checking data type of all variables

glimpse(lung_cancer_df)

# Removing records of people with negative test results

lung_cancer_df <-  lung_cancer_df %>% 
  filter(lung_cancer == 'YES')


# Converting all NO values to 0 and YES values to 1

lung_cancer_df$smoking <- as.double(mgsub(lung_cancer_df$smoking, c(1,2), c(0,1)))

lung_cancer_df$yellow_fingers <- as.double(mgsub(lung_cancer_df$yellow_fingers, c(1,2), c(0,1)))

lung_cancer_df$anxiety <- as.double(mgsub(lung_cancer_df$anxiety, c(1,2), c(0,1)))

lung_cancer_df$peer_pressure <- as.double(mgsub(lung_cancer_df$peer_pressure, c(1,2), c(0,1)))

lung_cancer_df$chronic_disease <- as.double(mgsub(lung_cancer_df$chronic_disease, c(1,2), c(0,1)))

lung_cancer_df$fatigue <- as.double(mgsub(lung_cancer_df$fatigue, c(1,2), c(0,1 )))

lung_cancer_df$allergy <- as.double(mgsub(lung_cancer_df$allergy, c(1,2), c(0,1 )))

lung_cancer_df$wheezing <- as.double(mgsub(lung_cancer_df$wheezing, c(1,2), c(0,1 )))

lung_cancer_df$alcohol_consuming <- as.double(mgsub(lung_cancer_df$alcohol_consuming, c(1,2), c(0,1 )))

lung_cancer_df$coughing <- as.double(mgsub(lung_cancer_df$coughing, c(1,2), c(0,1 )))

lung_cancer_df$shortness_of_breath <- as.double(mgsub(lung_cancer_df$shortness_of_breath, c(1,2), c(0,1 )))

lung_cancer_df$swallowing_difficulty <- as.double(mgsub(lung_cancer_df$swallowing_difficulty, c(1,2), c(0,1)))

lung_cancer_df$chest_pain <- as.double(mgsub(lung_cancer_df$chest_pain, c(1,2), c(0,1)))


# Create Connection to Sql Server database (rsample is the name of my dns)

conn <- odbcConnect("rsample", rows_at_time = 1)

# Save dataframe to database in SQL

sqlSave(conn, lung_cancer_df, "lung_cancer", append = TRUE, rownames = FALSE)

# Send query to extract first 10 rows of data

sqlQuery(conn, "SELECT TOP 10 * 
                FROM lung_cancer")

# Checking number of persons who tested positive(270)

sqlQuery(conn, "SELECT COUNT(*) 
                FROM lung_cancer")


# Check number of male and female in the data set (F = 125......M=145)

sqlQuery(conn, "SELECT gender,
                COUNT(*) AS 'total_persons'
                FROM lung_cancer 
                GROUP BY gender")



# Checking minimum, maximum and average age of men and women who tested positive for lung_cancer

sqlQuery(conn, "SELECT gender,
                COUNT (gender) as 'total_persons',
                ROUND(MIN(age),0) AS 'minimum_age',
                ROUND(MAX(age), 0) AS 'maximum_age',
                ROUND(AVG(age), 0) AS 'average_age'
                FROM lung_cancer 
                GROUP BY gender ")

# variables fall under 2 categories (Risk factors and  Symptoms)

# Reviewing risk factors for both genders

sqlQuery(conn, "WITH risk_factors AS 
                (SELECT gender,
                cancer_type,
                COUNT (gender) as 'total_persons',
                sum(smoking) AS 'smokers',
                sum(alcohol_consuming) AS 'drinkers'
                FROM lung_cancer
                GROUP BY gender, cancer_type )
         
                SELECT gender, 
                cancer_type,
                ROUND((smokers/total_persons)* 100, 0) AS 'percentage_smokers',
                ROUND((drinkers/total_persons)* 100, 0) AS 'percentage_drinkers'
                FROM risk_factors")

# Reviewing symptoms for both genders based on stage of lung cancer (chronic or not)

# Rename chronic disease to cancer type to depict if it is chronic or terminal

sqlQuery(conn, "sp_rename 'lung_cancer.chronic_disease', 'cancer_type', 'COLUMN'")

# Change data type of cancer type column to varchar(20)

sqlQuery(conn, "ALTER TABLE lung_cancer
                ALTER COLUMN cancer_type  varchar(20)")

# Change 1-chronic and 0-terminal

sqlQuery(conn, "UPDATE lung_cancer
                SET cancer_type  = 'TERMINAL'
                WHERE cancer_type = 0")

sqlQuery(conn, "UPDATE lung_cancer
                SET cancer_type  = 'CHRONIC'
                WHERE cancer_type = '1'")


sqlQuery(conn, "
                WITH symptoms_cte AS
                (SELECT gender,
                cancer_type,
                COUNT (cancer_type) AS 'total_persons',
                SUM(anxiety) AS 'anxiety',
                SUM(fatigue) AS 'fatigue',
                SUM(allergy) AS 'allergy',
                SUM(coughing) AS 'coughing',
                SUM(shortness_of_breath) AS 'shortness_of_breath',
                SUM(swallowing_difficulty) AS 'difficulty_swallowing',
                SUM(chest_pain) AS 'chest_pain',
                SUM(wheezing) AS 'wheezing'
                FROM lung_cancer
                GROUP BY cancer_type, gender )
         
                SELECT gender,
                cancer_type,
                ROUND((anxiety/total_persons)*100,0) AS percentage_anxiety,
                ROUND((fatigue/total_persons)*100, 0) AS percentage_fatigue,
                ROUND((allergy/total_persons)*100, 0) AS percentage_allergy,
                ROUND((coughing/total_persons)*100, 0) AS percentage_coughing,
                ROUND((shortness_of_breath/total_persons)*100, 0) AS percentage_short_breath,
                ROUND((difficulty_swallowing/total_persons)*100, 0) AS percentage_difficulty_swallowing,
                ROUND((chest_pain/total_persons)*100, 0) AS percentage_chest_pain,
                ROUND((wheezing/total_persons)*100, 0) AS percentage_wheezing
                FROM symptoms_cte")


odbcClose(conn)












