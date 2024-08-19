# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For date and time manipulation

# Load school data for Bristol and Cornwall
bristolSchools = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\bristol_schools_data.csv")
cornwallSchools = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\cornwall_schools_data.csv")

# Load crime rate data for Bristol and Cornwall
bristolCrime = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\bristol-crime-data.csv")
cornwallCrime = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\cornwall-crime-data.csv")

# Load population data for 2011
popn_2011 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\Population2011.csv")

# Adjust population data from 2011 to 2022 using an annual growth rate
population2022 = popn_2011 %>% 
  mutate(Population_2022 = Population * (1.00561255390388033 ^ (2022 - 2011)))

# Process crime rate data
bristolCrime = bristolCrime %>%
  mutate(Postcode = str_extract(postcode, "^\\S+ \\d")) %>%  # Extract the relevant part of the postcode
  mutate(Year = substring(Year, 1, 4))  # Extract the year

cornwallCrime = cornwallCrime %>%
  mutate(Postcode = str_extract(postcode, "^\\S+ \\d")) %>%  # Extract the relevant part of the postcode
  mutate(Year = substring(Year, 1, 4))  # Extract the year

# Calculate drug offense rates for Bristol in 2022
bristolDrugRates = bristolCrime %>%
  filter(Year == 2022) %>%  # Filter data for the year 2022
  left_join(population2022, by = "Postcode") %>%  # Join with population data
  filter(Crime_type == "Drugs") %>%  # Filter for drug-related crimes
  filter(!is.na(Population_2022)) %>%  # Remove rows with missing population data
  group_by(Postcode) %>%  # Group by postcode
  summarise(Drug_Offenses = n(),  # Count the number of drug offenses
            Population = first(Population_2022)) %>%  # Get the population for each postcode
  mutate(Drug_Offense_Rate = Drug_Offenses / Population * 10000)  # Calculate drug offense rate per 10,000 people

# Calculate drug offense rates for Cornwall in 2022
cornwallDrugRates = cornwallCrime %>%
  filter(Year == 2022) %>%  # Filter data for the year 2022
  left_join(population2022, by = "Postcode") %>%  # Join with population data
  filter(Crime_type == "Drugs") %>%  # Filter for drug-related crimes
  filter(!is.na(Population_2022)) %>%  # Remove rows with missing population data
  group_by(Postcode) %>%  # Group by postcode
  summarise(Drug_Offenses = n(),  # Count the number of drug offenses
            Population = first(Population_2022)) %>%  # Get the population for each postcode
  mutate(Drug_Offense_Rate = Drug_Offenses / Population * 10000)  # Calculate drug offense rate per 10,000 people

# Prepare school data
bristolSchools = bristolSchools %>%
  filter(Year == 2022) %>%  # Filter data for the year 2022
  mutate(Postcode = str_extract(Postcode, "^\\S+ \\d")) %>%  # Extract the relevant part of the postcode
  select(Postcode, ATT8SCR)  # Select postcode and Attainment 8 score

cornwallSchools = cornwallSchools %>%
  filter(Year == 2022) %>%  # Filter data for the year 2022
  mutate(Postcode = str_extract(Postcode, "^\\S+ \\d")) %>%  # Extract the relevant part of the postcode
  select(Postcode, ATT8SCR)  # Select postcode and Attainment 8 score

# Combine school data with crime data for Bristol
bristolAnalysis = inner_join(
  bristolSchools, 
  bristolDrugRates,
  by = "Postcode") %>%  # Join on postcode
  select(ATT8SCR, Drug_Offense_Rate)  # Select relevant columns

# Linear modeling for Bristol
bristolModel = lm(Drug_Offense_Rate ~ ATT8SCR, data = bristolAnalysis)  # Fit linear model
summary(bristolModel)  # Display model summary

# Plotting for Bristol
ggplot(bristolAnalysis, aes(x = ATT8SCR, y = Drug_Offense_Rate)) +
  geom_point(color = "blue", size = 2) +  # Plot data points
  geom_smooth(method = "lm", color = "red") +  # Add regression line
  labs(
    title = "Impact of Attainment 8 Scores on Drug Offense Rates in Bristol (2022)",  # Title
    x = "Attainment 8 Score",  # X-axis label
    y = "Drug Offense Rate (per 10,000)"  # Y-axis label
  ) +
  theme_minimal()  # Use minimal theme

# Combine school data with crime data for Cornwall
cornwallAnalysis = inner_join(
  cornwallSchools, 
  cornwallDrugRates,
  by = "Postcode") %>%  # Join on postcode
  select(ATT8SCR, Drug_Offense_Rate)  # Select relevant columns

# Linear modeling for Cornwall
cornwallModel = lm(Drug_Offense_Rate ~ ATT8SCR, data = cornwallAnalysis)  # Fit linear model
summary(cornwallModel)  # Display model summary

# Plotting for Cornwall
ggplot(cornwallAnalysis, aes(x = ATT8SCR, y = Drug_Offense_Rate)) +
  geom_point(color = "blue", size = 2) +  # Plot data points
  geom_smooth(method = "lm", color = "red") +  # Add regression line
  labs(
    title = "Impact of Attainment 8 Scores on Drug Offense Rates in Cornwall (2022)",  # Title
    x = "Attainment 8 Score",  # X-axis label
    y = "Drug Offense Rate (per 10,000)"  # Y-axis label
  ) +
  theme_minimal()  # Use minimal theme
