# Load necessary libraries
library(tidyverse)   # For data manipulation and visualization
library(lubridate)   # For handling date and time data
library(plotly)      # For interactive plots


# Load cleaned data sets from CSV files
# File paths are provided, and read_csv function loads the data into R
bristolHousePrice = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Housing Data\\house_pricing_bristol.csv")
cornwallHousePrice = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Housing Data\\house_pricing_cornwall.csv")
bcHousePrice = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Housing Data\\house_pricing_combined.csv")


# ---------------------------------------------------------

# Average House Price in Year 2022 (Boxplot) – For Both Counties

# Data Preparation
# Convert the 'Year' column from date format to just the year
bcHousePrice = bcHousePrice %>%
  mutate(Year = year(as.Date(Year))) %>%  # Convert 'Year' column to date and extract year
  filter(Year == 2022)  # Filter data to include only the year 2022

# Create a Box Plot to visualize house prices in 2022 for Bristol and Cornwall
# The 'options(scipen = 1000)' line prevents scientific notation on the axes
options(scipen = 1000)

HP22BoxPlot = ggplot(bcHousePrice, aes(x = County, y = Housing_Price)) + 
  geom_boxplot() +  # Add box plot layer
  labs(title = "House Prices of 2022 in Bristol and Cornwall",  # Add title and axis labels
       x = "County",
       y = "House Prices") +
  coord_cartesian(ylim = c(100000, 900000)) +  # Set y-axis limits to focus on a specific range of house prices
  theme_minimal()  # Use a minimal theme for the plot

# Convert the ggplot object to an interactive plotly object
ggplotly(HP22BoxPlot)

# ---------------------------------------------------------
  
# Average House Price in Year 2022 (Bar Chart) – For Both Counties

# Convert the 'Year' column from date format to just the year
bristolHousePrice = bristolHousePrice %>%
  mutate(Year = year(as.Date(Year))) %>%  # Extract year from date
  filter(Year == 2022) %>%  # Filter data to include only the year 2022
  mutate(Property_Type = recode(Type,  # Recode property types for clarity
                                "D" = "Detached",
                                "F" = "Flat",
                                "O" = "Other",
                                "S" = "Semi-Detached",
                                "T" = "Terraced"))

cornwallHousePrice = cornwallHousePrice %>%
  mutate(Year = year(as.Date(Year))) %>%  # Extract year from date
  filter(Year == 2022) %>%  # Filter data to include only the year 2022
  mutate(Property_Type = recode(Type,  # Recode property types for clarity
                                "D" = "Detached",
                                "F" = "Flat",
                                "O" = "Other",
                                "S" = "Semi-Detached",
                                "T" = "Terraced"))

# For Bristol County
bristolBarChart = bristolHousePrice %>%
  group_by(Town_City, Property_Type) %>%
  summarise(average_price = mean(Housing_Price, na.rm = TRUE)) %>%
  ggplot(aes(x = Property_Type, y = average_price, fill = Property_Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Town_City) +
  scale_fill_discrete(name = "Property Type") +
  labs(title = "Average House Prices of 2022 in Towns/Cities of Bristol",
       x = "Property Type",
       y = "Average House Price") +
  theme_minimal()

# For Cornwall County
cornwallBarChart = cornwallHousePrice %>%
  group_by(Town_City, Property_Type) %>%
  summarise(average_price = mean(Housing_Price, na.rm = TRUE)) %>%
  ggplot(aes(x = Property_Type, y = average_price, fill = Property_Type)) +
  geom_bar(stat = "identity", width = 0.4) +
  facet_wrap(~Town_City) +
  scale_fill_discrete(name = "Property Type") +
  labs(title = "Average House Prices of 2022 in Towns/Cities of Cornwall",
       x = "Property Type",
       y = "Average House Price") +
  theme_minimal() 

# Print both charts
print(bristolBarChart)
print(cornwallBarChart)

# ---------------------------------------------------------

# Average House Price From (2020 - 2023) – (Line Chart) 
# For both counties (Both Line chart should be shown in one single image)

# Prepare data for the line chart
bcHousePrice = bcHousePrice %>%
  mutate(Year = year(as.Date(Year))) %>%  # Ensure Year is in correct format
  filter(Year >= 2020 & Year <= 2023)  # Filter data for the years 2020 to 2023

# Create a line chart to visualize average house prices from 2020 to 2023
lineChart = bcHousePrice %>%
  group_by(Year, County) %>%
  summarise(average_price = mean(Housing_Price, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = average_price, color = County, group = County)) +
  geom_line() +  # Add line plot layer
  geom_point() +  # Add points to the lines
  labs(title = "Average House Prices from 2020 to 2023",
       x = "Year",
       y = "Average House Price") +
  theme_minimal()  # Use a minimal theme for the plot

# Convert the ggplot object to an interactive plotly object
ggplotly(lineChart)
