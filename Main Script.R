#Importing necessary libraries
library(readxl)
library(dplyr)

#Importing raw dataset
hackingData <- read.csv("hackingData.csv")
View(hackingData)

#Display the overview of the dataset
head(hackingData)
summary(hackingData)

#Store the invalid data in dataframe for cleaning
invalid_data_list <- list() # Initialize an empty list to store invalid columns

for (col in colnames(hackingData)) { # Loop through the columns of hackingData

  if (sum(is.na(hackingData[[col]])) != 0 || # Check if the column has N.A values
      sum(hackingData[[col]] == "") != 0 || # Check if the column has empty strings
      sum(hackingData[[col]] == "NULL") != 0 || # Check if the column has NULL value inside
      sum(hackingData[[col]] == "Unknown") != 0 # Check if the column has Unknown values
      ) { 
    invalid_data_list[[col]] <- hackingData[[col]]     # If the column has invalid data, add it to the list
  }
}

invalid_data_df <- as.data.frame(invalid_data_list, stringsAsFactors = FALSE) # Convert the list to a dataframe
colnames(invalid_data_df) # View the column names of the dataframe that contains invalid data


# Display the summary of the dataframe of invalid data
invalid_df_summary <- data.frame( # Set the data type of each column in the summary dataframe
  Column = character(),       # Column names
  Empty_Strings = integer(),  # Count of empty strings
  Unknown_Values = integer(), # Count of "unknown" values
  NA_Values = integer(),      # Count of NA values
  NULL_Values = integer(),      # Count of NULL values
  stringsAsFactors = FALSE
)

# Loop through each column in the dataset
for (col in colnames(invalid_data_df)) {
  column_data <- as.character(invalid_data_df[[col]]) #treat all column values as characters
  
  # Count occurrences of invalid values
  empty_count <- sum(column_data == "", na.rm = TRUE)      # Empty strings
  unknown_count <- sum(column_data == "unknown", na.rm = TRUE) # "unknown"
  na_count <- sum(is.na(column_data)) # NA values
  NULL_count <- sum(column_data == "NULL", na.rm = TRUE) # Null values
  
  # Append results to the summary data frame
  invalid_df_summary <- rbind(
    invalid_df_summary,
    data.frame(
      Column = col,
      Empty_Strings = empty_count,
      Unknown_Values = unknown_count,
      NA_Values = na_count,
      NULL_Values = NULL_count
    )
  )
}

invalid_df_summary # View the summary of the dataframe with invalid data

#Data Processing 
# Cleaning column Notify
# Replace missing values with mode

# Cleaning column URL
# Remove columns with no url

# Cleaning column IP
# Replace missing values with default IP address

# Cleaning column Country
# Replace missing values with mode

# Cleaning column OS
# Replace missing values with mode


# Cleaning column WebServer


# Cleaning column Encoding


# Cleaning column Lang


# Cleaning column Ransom


# Cleaning column Downtime


# Cleaning column Loss


#Removing Duplicates


#Removing Outliers


#Renaming Column Names


#Saving Cleaned Dataset in another csv file


