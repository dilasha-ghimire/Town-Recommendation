library(tidyverse)

# Load datasets from specified file paths
bristolData <- read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\bristol_schools_data.csv")  
cornwallData <- read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\cornwall_schools_data.csv")  
bcHousePrice <- read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_combined.csv")
bristol_crime <- read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\bristol-crime-data.csv")
cornwall_crime <- read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\cornwall-crime-data.csv")
combinedBroadbandData <- read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\combined_broadband_data.csv")
town_dataset <- read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\town_dataset.csv")

# Process Schools Data
schools_data <- bind_rows(bristolData, cornwallData) %>%
  group_by(Town) %>%  # Group data by Town
  summarise(Attainment8Score = mean(ATT8SCR, na.rm = TRUE)) %>%  # Calculate the average Attainment 8 score
  mutate(SchoolsScore = Attainment8Score / 9) %>%  # Normalize the score to a scale of 0-10
  select(Town, SchoolsScore)  # Select relevant columns

# Process House Prices Data
house_prices_data <- bcHousePrice %>%
  mutate(Year = as.numeric(format(as.Date(Year), "%Y"))) %>%  # Convert Year from date format to numeric year
  filter(Year == 2022) %>%  # Filter for the year 2022
  group_by(Town_City) %>%  # Group data by Town
  summarise(Price = mean(Housing_Price, na.rm = TRUE)) %>%  # Calculate the average house price
  mutate(HouseScore = 10 - (Price / 120000)) %>%  # Normalize the price to a scale of 0-10
  select(Town = Town_City, HouseScore)  # Rename column and select relevant columns

# Process Crime Data
crime_data <- bind_rows(bristol_crime, cornwall_crime) %>%
  filter(Year == "2022") %>%  # Filter for the year 2022
  filter(Crime_type %in% c("Violence and sexual offences", "Drugs", "Robbery", "Burglary")) %>%  # Filter relevant crime types
  mutate(CrimeScore = case_when(
    Crime_type %in% c("Drugs", "Violence and sexual offences") ~ 2,  # Assign scores for selected crime types
    Crime_type %in% c("Robbery", "Burglary") ~ 3
  )) %>%
  group_by(city) %>%  # Group data by city
  summarise(Rate = mean(CrimeScore, na.rm = TRUE)) %>%  # Calculate the average crime rate
  mutate(CrimeScore = 10 - (Rate / 10)) %>%  # Normalize the rate to a scale of 0-10
  rename(Town = city) %>%  # Rename column to match other datasets
  select(Town, CrimeScore)  # Select relevant columns

# Process Broadband Data
broadband_data <- combinedBroadbandData %>%
  group_by(postcode) %>%  # Group data by postcode
  summarise(AverageDownload = mean(`Average.download.speed..Mbit.s.`, na.rm = TRUE)) %>%  # Calculate average download speed
  mutate(DownloadScore = AverageDownload / 6) %>%  # Normalize the speed to a scale of 0-10
  rename(Town = postcode) %>%  # Rename column to match other datasets
  select(Town, DownloadScore)  # Select relevant columns

# Merge Town Data
towns_data <- town_dataset %>%
  rename(Town = Town_City, Short_Postcode = Short_Postcode)  # Rename columns to match other datasets

# Combine all data
final_data <- towns_data %>%
  left_join(schools_data, by = "Town") %>%  # Join with schools data
  left_join(house_prices_data, by = "Town") %>%  # Join with house prices data
  left_join(crime_data, by = "Town") %>%  # Join with crime data
  left_join(broadband_data, by = "Town") %>%  # Join with broadband data
  replace_na(list(SchoolsScore = 4, HouseScore = 4, CrimeScore = 4, DownloadScore = 4)) %>%  # Replace NA values with default score
  distinct(Town, .keep_all = TRUE) %>%  # Remove duplicate towns, keeping the first occurrence
  mutate(OverallScore = (HouseScore * 60) + (DownloadScore * 10) + (CrimeScore * 20) + (SchoolsScore * 10)) %>%  # Calculate overall score
  arrange(desc(OverallScore)) %>%  # Sort towns by overall score in descending order
  head(10)  # Select the top 10 towns

# Create a new dataset including only Town and OverallScore
recommended_town <- final_data %>%
  select(Town, OverallScore)

# Save the new dataset to a CSV file
write_csv(recommended_town, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Report\\TownScoreData.csv")

# Print the new dataset to console
recommended_town
