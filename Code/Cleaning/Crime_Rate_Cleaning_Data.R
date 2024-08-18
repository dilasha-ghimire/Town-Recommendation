# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(DescTools)  # For various statistical and utility functions

library(readr)      # Load the readr package for reading and writing CSV files
library(dplyr)      # Load the dplyr package for data manipulation


# Define a list of file paths for the crime data CSV files for 2021
file_paths_bs_2021 = list(
  "bs-2021-07" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2021-07\\2021-07-avon-and-somerset-street.csv",
  "bs-2021-08" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2021-08\\2021-08-avon-and-somerset-street.csv",
  "bs-2021-09" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2021-09\\2021-09-avon-and-somerset-street.csv",
  "bs-2021-10" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2021-10\\2021-10-avon-and-somerset-street.csv",
  "bs-2021-11" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2021-11\\2021-11-avon-and-somerset-street.csv",
  "bs-2021-12" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2021-12\\2021-12-avon-and-somerset-street.csv"
)

# Define a list of file paths for the crime data CSV files for 2022
file_paths_bs_2022 = list(
  "bs-2022-01" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-01\\2022-01-avon-and-somerset-street.csv",
  "bs-2022-02" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-02\\2022-02-avon-and-somerset-street.csv",
  "bs-2022-03" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-03\\2022-03-avon-and-somerset-street.csv",
  "bs-2022-04" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-04\\2022-04-avon-and-somerset-street.csv",
  "bs-2022-05" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-05\\2022-05-avon-and-somerset-street.csv",
  "bs-2022-06" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-06\\2022-06-avon-and-somerset-street.csv",
  "bs-2022-07" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-07\\2022-07-avon-and-somerset-street.csv",
  "bs-2022-08" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-08\\2022-08-avon-and-somerset-street.csv",
  "bs-2022-09" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-09\\2022-09-avon-and-somerset-street.csv",
  "bs-2022-10" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-10\\2022-10-avon-and-somerset-street.csv",
  "bs-2022-11" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-11\\2022-11-avon-and-somerset-street.csv",
  "bs-2022-12" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2022-12\\2022-12-avon-and-somerset-street.csv"
)

# Define a list of file paths for the crime data CSV files for 2023
file_paths_bs_2023 = list(
  "bs-2023-01" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-01\\2023-01-avon-and-somerset-street.csv",
  "bs-2023-02" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-02\\2023-02-avon-and-somerset-street.csv",
  "bs-2023-03" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-03\\2023-03-avon-and-somerset-street.csv",
  "bs-2023-04" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-04\\2023-04-avon-and-somerset-street.csv",
  "bs-2023-05" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-05\\2023-05-avon-and-somerset-street.csv",
  "bs-2023-06" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-06\\2023-06-avon-and-somerset-street.csv",
  "bs-2023-07" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-07\\2023-07-avon-and-somerset-street.csv",
  "bs-2023-08" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-08\\2023-08-avon-and-somerset-street.csv",
  "bs-2023-09" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-09\\2023-09-avon-and-somerset-street.csv",
  "bs-2023-10" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-10\\2023-10-avon-and-somerset-street.csv",
  "bs-2023-11" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-11\\2023-11-avon-and-somerset-street.csv",
  "bs-2023-12" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2023-12\\2023-12-avon-and-somerset-street.csv"
)

# Define a list of file paths for the crime data CSV files for 2024
file_paths_bs_2024 = list(
  "bs-2024-01" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2024-01\\2024-01-avon-and-somerset-street.csv",
  "bs-2024-02" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2024-02\\2024-02-avon-and-somerset-street.csv",
  "bs-2024-03" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2024-03\\2024-03-avon-and-somerset-street.csv",
  "bs-2024-04" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2024-04\\2024-04-avon-and-somerset-street.csv",
  "bs-2024-05" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2024-05\\2024-05-avon-and-somerset-street.csv",
  "bs-2024-06" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\avon-and-somerset-street\\2024-06\\2024-06-avon-and-somerset-street.csv"
)

# Function to read and clean each file
read_and_clean_bs = function(file_path) {
  df = read_csv(file_path)  # Read the CSV file
  # Remove rows with missing 'Crime ID'
  df = df %>% filter(!is.na(`Crime ID`))
  return(df)
}

# Define a function to rename columns
rename_columns_bs = function(df) {
  df = df %>%
    rename(
      Crime_ID = `Crime ID`,
      Reported_by = `Reported by`,
      Falls_Within = `Falls within`,
      LSOA_code = `LSOA code`,
      LSOA_name = `LSOA name`,
      Crime_type = `Crime type`,
      Last_outcome_category = `Last outcome category`
    )
  return(df)
}

# Apply the read_and_clean_bs function to all files in the list for 2021
crime_data_list_bs_2021 = lapply(file_paths_bs_2021, function(path) {
  df = read_and_clean_bs(path)
  df = rename_columns_bs(df)
  return(df)
})

# Combine all datasets into one dataframe for 2021
bsCrimeRate2021 = bind_rows(crime_data_list_bs_2021)
# Remove the 'Context' column as it is empty
bsCrimeRate2021 = bsCrimeRate2021 %>% select(-Context)
# Remove duplicated rows
bsCrimeRate2021 = bsCrimeRate2021 %>% distinct()
# Impute missing values
bsCrimeRate2021$Longitude[is.na(bsCrimeRate2021$Longitude)] = median(bsCrimeRate2021$Longitude, na.rm = TRUE)
bsCrimeRate2021$Latitude[is.na(bsCrimeRate2021$Latitude)] = median(bsCrimeRate2021$Latitude, na.rm = TRUE)
bsCrimeRate2021$LSOA_code[is.na(bsCrimeRate2021$LSOA_code)] = Mode(bsCrimeRate2021$LSOA_code, na.rm = TRUE)
bsCrimeRate2021$LSOA_name[is.na(bsCrimeRate2021$LSOA_name)] = Mode(bsCrimeRate2021$LSOA_name, na.rm = TRUE)
# Check if there are any remaining missing values
anyNA(bsCrimeRate2021)

# Apply the read_and_clean_bs function to all files in the list for 2022
crime_data_list_bs_2022 = lapply(file_paths_bs_2022, function(path) {
  df = read_and_clean_bs(path)
  df = rename_columns_bs(df)
  return(df)
})

# Combine all datasets into one dataframe for 2022
bsCrimeRate2022 = bind_rows(crime_data_list_bs_2022)
# Remove the 'Context' column as it is empty
bsCrimeRate2022 = bsCrimeRate2022 %>% select(-Context)
# Remove duplicated rows
bsCrimeRate2022 = bsCrimeRate2022 %>% distinct()
# Impute missing values
bsCrimeRate2022$Longitude[is.na(bsCrimeRate2022$Longitude)] = median(bsCrimeRate2022$Longitude, na.rm = TRUE)
bsCrimeRate2022$Latitude[is.na(bsCrimeRate2022$Latitude)] = median(bsCrimeRate2022$Latitude, na.rm = TRUE)
bsCrimeRate2022$LSOA_code[is.na(bsCrimeRate2022$LSOA_code)] = Mode(bsCrimeRate2022$LSOA_code, na.rm = TRUE)
bsCrimeRate2022$LSOA_name[is.na(bsCrimeRate2022$LSOA_name)] = Mode(bsCrimeRate2022$LSOA_name, na.rm = TRUE)
# Check if there are any remaining missing values
anyNA(bsCrimeRate2022)

# Apply the read_and_clean_bs function to all files in the list for 2023
crime_data_list_bs_2023 = lapply(file_paths_bs_2023, function(path) {
  df = read_and_clean_bs(path)
  df = rename_columns_bs(df)
  return(df)
})

# Combine all datasets into one dataframe for 2023
bsCrimeRate2023 = bind_rows(crime_data_list_bs_2023)
# Remove the 'Context' column as it is empty
bsCrimeRate2023 = bsCrimeRate2023 %>% select(-Context)
# Remove duplicated rows
bsCrimeRate2023 = bsCrimeRate2023 %>% distinct()
# Impute missing values
bsCrimeRate2023$Longitude[is.na(bsCrimeRate2023$Longitude)] = median(bsCrimeRate2023$Longitude, na.rm = TRUE)
bsCrimeRate2023$Latitude[is.na(bsCrimeRate2023$Latitude)] = median(bsCrimeRate2023$Latitude, na.rm = TRUE)
bsCrimeRate2023$LSOA_code[is.na(bsCrimeRate2023$LSOA_code)] = Mode(bsCrimeRate2023$LSOA_code, na.rm = TRUE)
bsCrimeRate2023$LSOA_name[is.na(bsCrimeRate2023$LSOA_name)] = Mode(bsCrimeRate2023$LSOA_name, na.rm = TRUE)
# Check if there are any remaining missing values
anyNA(bsCrimeRate2023)

# Apply the read_and_clean_bs function to all files in the list for 2024
crime_data_list_bs_2024 = lapply(file_paths_bs_2024, function(path) {
  df = read_and_clean_bs(path)
  df = rename_columns_bs(df)
  return(df)
})

# Combine all datasets into one dataframe for 2024
bsCrimeRate2024 = bind_rows(crime_data_list_bs_2024)
# Remove the 'Context' column as it is empty
bsCrimeRate2024 = bsCrimeRate2024 %>% select(-Context)
# Remove duplicated rows
bsCrimeRate2024 = bsCrimeRate2024 %>% distinct()
# Impute missing values
bsCrimeRate2024$Longitude[is.na(bsCrimeRate2024$Longitude)] = median(bsCrimeRate2024$Longitude, na.rm = TRUE)
bsCrimeRate2024$Latitude[is.na(bsCrimeRate2024$Latitude)] = median(bsCrimeRate2024$Latitude, na.rm = TRUE)
bsCrimeRate2024$LSOA_code[is.na(bsCrimeRate2024$LSOA_code)] = Mode(bsCrimeRate2024$LSOA_code, na.rm = TRUE)
bsCrimeRate2024$LSOA_name[is.na(bsCrimeRate2024$LSOA_name)] = Mode(bsCrimeRate2024$LSOA_name, na.rm = TRUE)
# Check if there are any remaining missing values
anyNA(bsCrimeRate2024)


# -----------------------------------------------------


# Define a list of file paths for the crime data CSV files for 2021
file_paths_cornwall_2021 = list(
  "dc-2021-07" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2021-07\\2021-07-devon-and-cornwall-street.csv",
  "dc-2021-08" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2021-08\\2021-08-devon-and-cornwall-street.csv",
  "dc-2021-09" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2021-09\\2021-09-devon-and-cornwall-street.csv",
  "dc-2021-10" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2021-10\\2021-10-devon-and-cornwall-street.csv",
  "dc-2021-11" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2021-11\\2021-11-devon-and-cornwall-street.csv",
  "dc-2021-12" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2021-12\\2021-12-devon-and-cornwall-street.csv"
)

# Define a list of file paths for the crime data CSV files for 2022
file_paths_cornwall_2022 = list(
  "dc-2022-01" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-01\\2022-01-devon-and-cornwall-street.csv",
  "dc-2022-02" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-02\\2022-02-devon-and-cornwall-street.csv",
  "dc-2022-03" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-03\\2022-03-devon-and-cornwall-street.csv",
  "dc-2022-04" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-04\\2022-04-devon-and-cornwall-street.csv",
  "dc-2022-05" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-05\\2022-05-devon-and-cornwall-street.csv",
  "dc-2022-06" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-06\\2022-06-devon-and-cornwall-street.csv",
  "dc-2022-07" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-07\\2022-07-devon-and-cornwall-street.csv",
  "dc-2022-08" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-08\\2022-08-devon-and-cornwall-street.csv",
  "dc-2022-09" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-09\\2022-09-devon-and-cornwall-street.csv",
  "dc-2022-10" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-10\\2022-10-devon-and-cornwall-street.csv",
  "dc-2022-11" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-11\\2022-11-devon-and-cornwall-street.csv",
  "dc-2022-12" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2022-12\\2022-12-devon-and-cornwall-street.csv"
)

# Define a list of file paths for the crime data CSV files for 2023
file_paths_cornwall_2023 = list(
  "dc-2023-01" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-01\\2023-01-devon-and-cornwall-street.csv",
  "dc-2023-02" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-02\\2023-02-devon-and-cornwall-street.csv",
  "dc-2023-03" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-03\\2023-03-devon-and-cornwall-street.csv",
  "dc-2023-04" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-04\\2023-04-devon-and-cornwall-street.csv",
  "dc-2023-05" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-05\\2023-05-devon-and-cornwall-street.csv",
  "dc-2023-06" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-06\\2023-06-devon-and-cornwall-street.csv",
  "dc-2023-07" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-07\\2023-07-devon-and-cornwall-street.csv",
  "dc-2023-08" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-08\\2023-08-devon-and-cornwall-street.csv",
  "dc-2023-09" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-09\\2023-09-devon-and-cornwall-street.csv",
  "dc-2023-10" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-10\\2023-10-devon-and-cornwall-street.csv",
  "dc-2023-11" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-11\\2023-11-devon-and-cornwall-street.csv",
  "dc-2023-12" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2023-12\\2023-12-devon-and-cornwall-street.csv"
)

# Define a list of file paths for the crime data CSV files for 2024
file_paths_cornwall_2024 = list(
  "dc-2024-01" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2024-01\\2024-01-devon-and-cornwall-street.csv",
  "dc-2024-02" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2024-02\\2024-02-devon-and-cornwall-street.csv",
  "dc-2024-03" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2024-03\\2024-03-devon-and-cornwall-street.csv",
  "dc-2024-04" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2024-04\\2024-04-devon-and-cornwall-street.csv",
  "dc-2024-05" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2024-05\\2024-05-devon-and-cornwall-street.csv",
  "dc-2024-06" = "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\devon-and-cornwall-street\\2024-06\\2024-06-devon-and-cornwall-street.csv"
)

# Function to read and clean each file for Cornwall data
read_and_clean_cornwall = function(file_path) {
  df = read_csv(file_path)  # Read the CSV file
  # Remove rows with missing 'Crime ID'
  df = df %>% filter(!is.na(`Crime ID`))
  return(df)
}

# Function to rename columns for Cornwall data
rename_columns_cornwall = function(df) {
  df = df %>%
    rename(
      Crime_ID = `Crime ID`,
      Reported_by = `Reported by`,
      Falls_Within = `Falls within`,
      LSOA_code = `LSOA code`,
      LSOA_name = `LSOA name`,
      Crime_type = `Crime type`,
      Last_outcome_category = `Last outcome category`
    )
  return(df)
}

# Apply the read_and_clean_cornwall function to all files in the list for 2021
crime_data_list_cornwall_2021 = lapply(file_paths_cornwall_2021, function(path) {
  df = read_and_clean_cornwall(path)
  df = rename_columns_cornwall(df)
  return(df)
})

# Combine all datasets into one dataframe for 2021
cornwallCrimeRate2021 = bind_rows(crime_data_list_cornwall_2021)
# Remove the 'Context' column as it is empty
cornwallCrimeRate2021 = cornwallCrimeRate2021 %>% select(-Context)
# Remove duplicated rows
cornwallCrimeRate2021 = cornwallCrimeRate2021 %>% distinct()
# Impute missing values
cornwallCrimeRate2021$Longitude[is.na(cornwallCrimeRate2021$Longitude)] = median(cornwallCrimeRate2021$Longitude, na.rm = TRUE)
cornwallCrimeRate2021$Latitude[is.na(cornwallCrimeRate2021$Latitude)] = median(cornwallCrimeRate2021$Latitude, na.rm = TRUE)
cornwallCrimeRate2021$LSOA_code[is.na(cornwallCrimeRate2021$LSOA_code)] = Mode(cornwallCrimeRate2021$LSOA_code, na.rm = TRUE)
cornwallCrimeRate2021$LSOA_name[is.na(cornwallCrimeRate2021$LSOA_name)] = Mode(cornwallCrimeRate2021$LSOA_name, na.rm = TRUE)
# Check if there are any remaining missing values
anyNA(cornwallCrimeRate2021)

# Apply the read_and_clean_cornwall function to all files in the list for 2022
crime_data_list_cornwall_2022 = lapply(file_paths_cornwall_2022, function(path) {
  df = read_and_clean_cornwall(path)
  df = rename_columns_cornwall(df)
  return(df)
})

# Combine all datasets into one dataframe for 2022
cornwallCrimeRate2022 = bind_rows(crime_data_list_cornwall_2022)
# Remove the 'Context' column as it is empty
cornwallCrimeRate2022 = cornwallCrimeRate2022 %>% select(-Context)
# Remove duplicated rows
cornwallCrimeRate2022 = cornwallCrimeRate2022 %>% distinct()
# Impute missing values
cornwallCrimeRate2022$Longitude[is.na(cornwallCrimeRate2022$Longitude)] = median(cornwallCrimeRate2022$Longitude, na.rm = TRUE)
cornwallCrimeRate2022$Latitude[is.na(cornwallCrimeRate2022$Latitude)] = median(cornwallCrimeRate2022$Latitude, na.rm = TRUE)
cornwallCrimeRate2022$LSOA_code[is.na(cornwallCrimeRate2022$LSOA_code)] = Mode(cornwallCrimeRate2022$LSOA_code, na.rm = TRUE)
cornwallCrimeRate2022$LSOA_name[is.na(cornwallCrimeRate2022$LSOA_name)] = Mode(cornwallCrimeRate2022$LSOA_name, na.rm = TRUE)
# Check if there are any remaining missing values
anyNA(cornwallCrimeRate2022)

# Apply the read_and_clean_cornwall function to all files in the list for 2023
crime_data_list_cornwall_2023 = lapply(file_paths_cornwall_2023, function(path) {
  df = read_and_clean_cornwall(path)
  df = rename_columns_cornwall(df)
  return(df)
})

# Combine all datasets into one dataframe for 2023
cornwallCrimeRate2023 = bind_rows(crime_data_list_cornwall_2023)
# Remove the 'Context' column as it is empty
cornwallCrimeRate2023 = cornwallCrimeRate2023 %>% select(-Context)
# Remove duplicated rows
cornwallCrimeRate2023 = cornwallCrimeRate2023 %>% distinct()
# Impute missing values
cornwallCrimeRate2023$Longitude[is.na(cornwallCrimeRate2023$Longitude)] = median(cornwallCrimeRate2023$Longitude, na.rm = TRUE)
cornwallCrimeRate2023$Latitude[is.na(cornwallCrimeRate2023$Latitude)] = median(cornwallCrimeRate2023$Latitude, na.rm = TRUE)
cornwallCrimeRate2023$LSOA_code[is.na(cornwallCrimeRate2023$LSOA_code)] = Mode(cornwallCrimeRate2023$LSOA_code, na.rm = TRUE)
cornwallCrimeRate2023$LSOA_name[is.na(cornwallCrimeRate2023$LSOA_name)] = Mode(cornwallCrimeRate2023$LSOA_name, na.rm = TRUE)
# Check if there are any remaining missing values
anyNA(cornwallCrimeRate2023)

# Apply the read_and_clean_cornwall function to all files in the list for 2024
crime_data_list_cornwall_2024 = lapply(file_paths_cornwall_2024, function(path) {
  df = read_and_clean_cornwall(path)
  df = rename_columns_cornwall(df)
  return(df)
})

# Combine all datasets into one dataframe for 2024
cornwallCrimeRate2024 = bind_rows(crime_data_list_cornwall_2024)
# Remove the 'Context' column as it is empty
cornwallCrimeRate2024 = cornwallCrimeRate2024 %>% select(-Context)
# Remove duplicated rows
cornwallCrimeRate2024 = cornwallCrimeRate2024 %>% distinct()
# Impute missing values
cornwallCrimeRate2024$Longitude[is.na(cornwallCrimeRate2024$Longitude)] = median(cornwallCrimeRate2024$Longitude, na.rm = TRUE)
cornwallCrimeRate2024$Latitude[is.na(cornwallCrimeRate2024$Latitude)] = median(cornwallCrimeRate2024$Latitude, na.rm = TRUE)
cornwallCrimeRate2024$LSOA_code[is.na(cornwallCrimeRate2024$LSOA_code)] = Mode(cornwallCrimeRate2024$LSOA_code, na.rm = TRUE)
cornwallCrimeRate2024$LSOA_name[is.na(cornwallCrimeRate2024$LSOA_name)] = Mode(cornwallCrimeRate2024$LSOA_name, na.rm = TRUE)
# Check if there are any remaining missing values
anyNA(cornwallCrimeRate2024)


# -----------------------------------------------------


# Combine Bristol crime rate datasets
bristolCrimeData = bind_rows(bsCrimeRate2021, bsCrimeRate2022, bsCrimeRate2023, bsCrimeRate2024)
View(bristolCrimeData)

# Combine Cornwall crime rate datasets
cornwallCrimeData = bind_rows(cornwallCrimeRate2021, cornwallCrimeRate2022, cornwallCrimeRate2023, cornwallCrimeRate2024)
View(cornwallCrimeData)

# For adding town/city to the data set
postcodeToLSOA = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Crime Rate Data\\Postcode to LSOA.csv")
View(postcodeToLSOA)

# The dataset is narrowed down to necessary columns
postcodeToLSOA = postcodeToLSOA %>% 
  rename(postcode = pcds) %>% 
  rename(LSOA_code = lsoa11cd) %>% 
  rename(city = ladnm) %>% 
  select(postcode, LSOA_code, city)
View(postcodeToLSOA)

# Remove duplicated associations and keep only the first occurrence
postcodeToLSOA = postcodeToLSOA %>% 
  group_by(LSOA_code) %>% 
  summarize(
    postcode = first(postcode), 
    city = first(city), 
    .groups = 'drop'
  )
View(postcodeToLSOA)

# Inner join done for Bristol crime data and postcodeToLSOA, only retaining common rows
bristolCrimeData = bristolCrimeData %>% 
  rename(Year = Month) %>% 
  select(Crime_ID, Year, LSOA_code, LSOA_name, Crime_type) %>% 
  inner_join(postcodeToLSOA, by = "LSOA_code")
View(bristolCrimeData)

# Inner join done for Cornwall crime data and postcodeToLSOA
cornwallCrimeData = cornwallCrimeData %>% 
  rename(Year = Month) %>% 
  select(Crime_ID, Year, LSOA_code, LSOA_name, Crime_type) %>% 
  inner_join(postcodeToLSOA, by = "LSOA_code")
View(cornwallCrimeData)

# Save cleaned datasets and summaries
write.csv(bristolCrimeData, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\bristol-crime-data.csv", row.names = FALSE)
write.csv(cornwallCrimeData, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\cornwall-crime-data.csv", row.names = FALSE)
