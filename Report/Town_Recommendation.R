# Load necessary libraries
library(dplyr)   
library(tidyr)   
library(scales)  
library(purrr)   
library(readr)

# Read in the datasets
house_data_bristol = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_bristol.csv")
house_data_cornwall = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_cornwall.csv")
broadband_bristol = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\bristol_broadband_data.csv")
broadband_cornwall = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\cornwall_broadband_data.csv")
crime_bristol = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\bristol-crime-data.csv")
crime_cornwall = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\cornwall-crime-data.csv")
schools_bristol = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\bristol_schools_data.csv")  
schools_cornwall = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\cornwall_schools_data.csv")  

# Rename columns to ensure consistency across datasets
crime_bristol = crime_bristol %>%
  rename(Postcode = postcode, Town = city)
crime_cornwall = crime_cornwall %>%
  rename(Postcode = postcode, Town = city)
house_data_bristol = house_data_bristol %>%
  rename(Postcode = Postcode, Town = Town_City)
house_data_cornwall = house_data_cornwall %>%
  rename(Postcode = Postcode, Town = Town_City)
broadband_bristol = broadband_bristol %>%
  rename(Postcode = postcode_space, Town = county)
broadband_cornwall = broadband_cornwall %>%
  rename(Postcode = postcode_space, Town = county)
schools_bristol = schools_bristol %>%
  rename(Postcode = Postcode, Town = Town)
schools_cornwall = schools_cornwall %>%
  rename(Postcode = Postcode, Town = Town)

# Combine Bristol datasets
combined_bristol = reduce(list(house_data_bristol, broadband_bristol, crime_bristol, schools_bristol), full_join, by = c("Town", "Postcode"))

# Combine Cornwall datasets
combined_cornwall = reduce(list(house_data_cornwall, broadband_cornwall, crime_cornwall, schools_cornwall), full_join, by = c("Town", "Postcode"))

# Combine Bristol and Cornwall data
combined_data = bind_rows(combined_bristol, combined_cornwall)

# Function to calculate a final score based on crime rate, school quality, and house prices
calculate_final_score = function(data) {
  data %>%
    mutate(
      Housing_Price = ifelse(is.na(Housing_Price), median(data$Housing_Price, na.rm = TRUE), Housing_Price),
      crime_rate = ifelse(is.na(Crime_ID), 0, 1), 
      school_quality = ifelse(is.na(OFSTEDRATING), 0, as.numeric(OFSTEDRATING)),
      crime_rate_score = 10 * (1 - rescale(crime_rate, to = c(0, 1))),
      school_quality_score = 10 * rescale(school_quality, to = c(0, 1)),
      house_price_score = 10 * (1 - rescale(Housing_Price, to = c(0, 1))),
      final_score = 0.4 * crime_rate_score + 0.3 * school_quality_score + 0.3 * house_price_score
    )
}

# Function to the combined data
combined_data_with_scores = calculate_final_score(combined_data)

# Filter top recommendations based on the highest final score
top_recommendations = combined_data_with_scores %>%
  arrange(desc(final_score)) %>%
  select(ID, Postcode, Town, County.x, final_score) %>%
  distinct(Town, .keep_all = TRUE) %>%
  head(10)

write.csv(top_recommendations, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Report\\TownScoreData.csv", row.names = FALSE)

