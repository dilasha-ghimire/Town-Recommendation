# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For handling date data

# Load cleaned house pricing data for combined, Bristol, and Cornwall
house_pricing_data_combined = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_combined.csv")
house_pricing_data_bristol = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_bristol.csv")
house_pricing_data_cornwall = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_cornwall.csv")

# Load cleaned broadband data for Bristol, Cornwall, and combined
bristol_broadband_data = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\bristol_broadband_data.csv")
cornwall_broadband_data = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\cornwall_broadband_data.csv")
combined_broadband_data = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\combined_broadband_data.csv")

# Convert 'Year' column to date format and extract the year for house pricing data
house_pricing_data_bristol = house_pricing_data_bristol %>%
  mutate(Year = year(ymd(Year)))  # Convert 'Year' to numeric year

house_pricing_data_cornwall = house_pricing_data_cornwall %>%
  mutate(Year = year(ymd(Year)))  # Convert 'Year' to numeric year

# Convert house prices to thousands
house_pricing_data_bristol = house_pricing_data_bristol %>%
  mutate(Housing_Price = Housing_Price / 10000)  # Convert prices to thousands

house_pricing_data_cornwall = house_pricing_data_cornwall %>%
  mutate(Housing_Price = Housing_Price / 10000)  # Convert prices to thousands

# Check for matching postcodes between house pricing and broadband data for Bristol
matching_postcodes_bristol = intersect(house_pricing_data_bristol$Postcode, bristol_broadband_data$postcode_space)
print(length(matching_postcodes_bristol))  # Should be greater than zero, indicating matches

# Check for matching postcodes between house pricing and broadband data for Cornwall
matching_postcodes_cornwall = intersect(house_pricing_data_cornwall$Postcode, cornwall_broadband_data$postcode_space)
print(length(matching_postcodes_cornwall))  # Should be greater than zero, indicating matches

# Check if there are rows for the year 2022 in the house pricing data for Bristol
year_check_bristol = house_pricing_data_bristol %>% filter(Year == 2022)
print(nrow(year_check_bristol))  # Should be greater than zero, indicating data for 2022 is present

# Check if there are rows for the year 2022 in the house pricing data for Cornwall
year_check_cornwall = house_pricing_data_cornwall %>% filter(Year == 2022)
print(nrow(year_check_cornwall))  # Should be greater than zero, indicating data for 2022 is present

# Combine house pricing data with broadband data for Bristol for the year 2022
bristol_data_2022 = inner_join(
  house_pricing_data_bristol,
  bristol_broadband_data,
  by = c("Postcode" = "postcode_space")  # Merge on matching postcodes
) %>%
  filter(Year == 2022) %>%
  select(Housing_Price, `Average.download.speed..Mbit.s.`)  # Select relevant columns

# Display the number of rows in the merged Bristol dataset for 2022
print(nrow(bristol_data_2022))  # Should be greater than zero, indicating successful merge

# Calculate correlation for Bristol dataset
correlation_bristol = bristol_data_2022 %>%
  summarise(corCoeff = cor(Housing_Price, `Average.download.speed..Mbit.s.`))  # Calculate correlation
print(correlation_bristol)  # Print correlation coefficient

# Negative but weak relationship (-0.0131)

# Build and summarize linear model for Bristol dataset
model_bristol = lm(Housing_Price ~ `Average.download.speed..Mbit.s.`, data = bristol_data_2022)  # Linear regression
summary(model_bristol)  # Summary of the model

# Extract intercept and slope for the regression line
intercept_bristol = coef(model_bristol)[1]  # Intercept
slope_bristol = coef(model_bristol)[2]  # Slope

# Plot the data and regression line for Bristol with house prices in thousands
ggplot(bristol_data_2022, aes(x = `Average.download.speed..Mbit.s.`, y = Housing_Price)) +
  geom_point(color = "blue", size = 2) +  # Plot data points
  geom_abline(intercept = intercept_bristol, slope = slope_bristol, color = "red") +  # Plot regression line
  labs(
    title = "Impact of Average Download Speed on House Price in Bristol (2022)",
    x = "Average Download Speed (Mbit/s)",
    y = "House Price (£1000s)"  # Update y-axis label
  ) +
  theme_minimal()  # Minimal theme for plot

# Combine house pricing data with broadband data for Cornwall for the year 2022
cornwall_data_2022 = inner_join(
  house_pricing_data_cornwall,
  cornwall_broadband_data,
  by = c("Postcode" = "postcode_space")  # Merge on matching postcodes
) %>%
  filter(Year == 2022) %>%
  select(Housing_Price, `Average.download.speed..Mbit.s.`)  # Select relevant columns

# Display the number of rows in the merged Cornwall dataset for 2022
print(nrow(cornwall_data_2022))  # Should be greater than zero, indicating successful merge

# Calculate correlation for Cornwall dataset
correlation_cornwall = cornwall_data_2022 %>%
  summarise(corCoeff = cor(Housing_Price, `Average.download.speed..Mbit.s.`))  # Calculate correlation
print(correlation_cornwall)  # Print correlation coefficient

# Negative but weak relationship (-0.0194)

# Build and summarize linear model for Cornwall dataset
model_cornwall = lm(Housing_Price ~ `Average.download.speed..Mbit.s.`, data = cornwall_data_2022)  # Linear regression
summary(model_cornwall)  # Summary of the model

# Extract intercept and slope for the regression line
intercept_cornwall = coef(model_cornwall)[1]  # Intercept
slope_cornwall = coef(model_cornwall)[2]  # Slope

# Plot the data and regression line for Cornwall with house prices in thousands
ggplot(cornwall_data_2022, aes(x = `Average.download.speed..Mbit.s.`, y = Housing_Price)) +
  geom_point(color = "blue", size = 2) +  # Plot data points
  geom_abline(intercept = intercept_cornwall, slope = slope_cornwall, color = "red") +  # Plot regression line
  labs(
    title = "Impact of Average Download Speed on House Price in Cornwall (2022)",
    x = "Average Download Speed (Mbit/s)",
    y = "House Price (£1000s)"  # Update y-axis label
  ) +
  theme_minimal()  # Minimal theme for plot
