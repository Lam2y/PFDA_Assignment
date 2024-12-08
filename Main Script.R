#Importing necessary libraries
library(readxl)
library(dplyr)
library(tidyr)  

#Importing raw dataset
hackingData <- read.csv("hackingData.csv")
View(hackingData) #View dataset

#Display the overview of the dataset
head(hackingData) # See the first 5 columns of the dataset
summary(hackingData) # See the overall statistics of the dataset

#Data Processing 

# Display the summary of the data frame of invalid data
invalid_data_summary <- data.frame( # Set the data type of each column in the summary data frame
  Column = character(),       # Column names
  Empty_Strings = integer(),  # Count of empty strings
  Unknown_Values = integer(), # Count of "unknown" values
  NA_Values = integer(),      # Count of NA values
  NULL_Values = integer(),      # Count of NULL values
  stringsAsFactors = FALSE
)

for (col in colnames(hackingData)) { # Loop through each column in the invalid data data frame
  hackingData$Notify <- iconv(hackingData$Notify, from = "UTF-8", to = "UTF-8", sub = "byte") # Clean invalid UTF-8 characters in the Notify column
  column_data <- as.character(hackingData[[col]])  #treat all column values as characters
  empty_count <- sum(trimws(hackingData[[col]]) == "", na.rm = TRUE)      # Count occurrences of Empty strings
  unknown_count <-  sum(grepl("^(unknown)$", hackingData[[col]], ignore.case = TRUE)) # Count occurrences of "unknown"
  na_count <- sum(is.na(column_data)) # Count occurrences of NA values
  NULL_count <- sum(is.null(column_data) | grepl("^(null)$", hackingData[[col]], ignore.case = TRUE)) # Count occurrences of Null values
  
  invalid_data_summary <- rbind( # Bind results to the summary data frame
    invalid_data_summary,
    data.frame(
      Column = col, 
      Empty_Strings = empty_count,
      Unknown_Values = unknown_count,
      NA_Values = na_count,
      NULL_Values = NULL_count
    )
  )
}

invalid_data_summary
invalid_data_summary
invalid_data_summary # View the summary of the data frame with invalid data

# Cleaning column Notify
get_mode <- function(v) {
  v_cleaned <- v[!(is.na(v) | v == "" | grepl("^(unknown)$", v, ignore.case = TRUE) | grepl("^(null)$", v, ignore.case = TRUE) | is.null(v))]
  # Remove NA and empty strings from the vector
  uniq_vals <- unique(v_cleaned) # Get unique values from the cleaned vector
  uniq_vals[which.max(tabulate(match(v_cleaned, uniq_vals)))] # Return the mode (the most frequent value)
}

hackingData <- hackingData %>%
  mutate(Notify = if_else((trimws(Notify) == ""|grepl("^(unknown)$", Notify, ignore.case = TRUE)), # Replace invalid data with mode
                          get_mode(Notify), Notify))

# Cleaning column URL
hackingData <- hackingData %>% 
  filter(URL != "")  # Filter invalid data 


# Cleaning column IP
default_ip <- "192.168.0.1"
hackingData <- hackingData %>%
  mutate(IP = if_else(trimws(IP) == "" | is.null(IP), default_ip, IP)) # Replacing invalid data with default IP address

# Cleaning column Country
hackingData <- hackingData %>%
  mutate(Country = if_else((trimws(Country) == ""|grepl("^(unknown)$", Country, ignore.case = TRUE)), get_mode(Country), Country)) # Replace invalid data with mode

# Cleaning column OS
hackingData <- hackingData %>%
  mutate(OS = if_else((trimws(OS) == ""|grepl("^(unknown)$", OS, ignore.case = TRUE)), get_mode(OS), OS)) # Replace invalid data with mode

# Cleaning column WebServer


# Cleaning column Encoding


# Cleaning column Lang


# Cleaning column Ransom


# Cleaning column Loss


#Removing Duplicates


#Removing Outliers


#Renaming Column Names


#Saving Cleaned Dataset in another csv file


