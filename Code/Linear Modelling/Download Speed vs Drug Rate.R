library(tidyverse) # Load the tidyverse package for data manipulation and visualization
library(lubridate) # Load lubridate for date manipulation

# Load broadband speed data for Bristol and Cornwall
bristolBroadband = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\bristol_broadband_data.csv")
cornwallBroadband = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\cornwall_broadband_data.csv")

# Load crime rate data for Bristol and Cornwall
bristolCrime = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\bristol-crime-data.csv")
cornwallCrime = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\cornwall-crime-data.csv")

# Load population data for 2011
population2011 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\Population2011.csv")

# Adjust population data from 2011 to 2023
population2023 = population2011 %>% 
  mutate(Population_2023 = Population * 1.00561255390388033) # Apply growth factor to estimate population in 2023

# Calculate annual growth rate and adjust population data to 2022
years_span = 2023 - 2011 # Calculate the number of years between 2011 and 2023
growth_rate = 1.00561255390388033 ^ (1 / years_span) # Compute the annual growth rate
population2022 = population2023 %>% 
  mutate(Population_2022 = Population_2023 / growth_rate) # Adjust population data to estimate 2022 population

# Process crime rate data for Bristol
bristolCrime = bristolCrime %>%
  mutate(Postcode = str_extract(postcode, "^\\S+ \\d")) %>% # Extract postcode (up to the first space)
  mutate(Year = substring(Year, 1, 4)) # Extract only the year from the date

# Process crime rate data for Cornwall
cornwallCrime = cornwallCrime %>%
  mutate(Postcode = str_extract(postcode, "^\\S+ \\d")) %>% # Extract postcode (up to the first space)
  mutate(Year = substring(Year, 1, 4)) # Extract only the year from the date

# Calculate drug offense rates for Bristol in 2022
bristolDrugRates = bristolCrime %>% 
  filter(Year == 2022) %>% # Filter data for the year 2022
  left_join(population2022, by = "Postcode") %>% # Join with population data for 2022
  filter(Crime_type == "Drugs") %>% # Filter for drug-related crimes
  filter(city == "Bristol, City of") %>% # Filter for Bristol city
  filter(!is.na(Population_2022)) %>% # Exclude rows where population data is missing
  group_by(postcode) %>% # Group data by postcode
  summarise(Drug_Offenses = n(), # Count the number of drug offenses
            Population = first(Population_2022)) %>% # Get the population for each postcode
  mutate(Drug_Offense_Rate = Drug_Offenses / Population * 10000) # Calculate the drug offense rate per 10,000 people

# View the result to troubleshoot
View(bristolDrugRates)

# Calculate drug offense rates for Cornwall in 2022
cornwallDrugRates = cornwallCrime %>% 
  filter(Year == 2022) %>% # Filter data for the year 2022
  left_join(population2022, by = "Postcode") %>% # Join with population data for 2022
  filter(Crime_type == "Drugs") %>% # Filter for drug-related crimes
  filter(city == "Cornwall") %>% # Filter for Cornwall
  filter(!is.na(Population_2022)) %>% # Exclude rows where population data is missing
  group_by(postcode) %>% # Group data by postcode
  summarise(Drug_Offenses = n(), # Count the number of drug offenses
            Population = first(Population_2022)) %>% # Get the population for each postcode
  mutate(Drug_Offense_Rate = Drug_Offenses / Population * 10000) # Calculate the drug offense rate per 10,000 people

# Linear Modeling for Bristol
bristolAnalysis = inner_join(
  bristolBroadband, 
  bristolDrugRates,
  by = c("postcode_space" = "postcode")) %>% # Join broadband and crime rate data on postcode
  select(`Average.download.speed..Mbit.s.`, Drug_Offense_Rate) # Select columns for analysis

# Calculate correlation and fit linear model for Bristol
correlation_bristol = bristolAnalysis %>% 
  summarise(correlation = cor(Drug_Offense_Rate, `Average.download.speed..Mbit.s.`)) # Compute correlation between broadband speed and drug offense rate
print(correlation_bristol) # Print the correlation value

bristolModel = lm(Drug_Offense_Rate ~ `Average.download.speed..Mbit.s.`, data = bristolAnalysis) # Fit a linear model
summary(bristolModel) # Print the summary of the linear model

bristolIntercept = coef(bristolModel)[1] # Extract the intercept from the model
bristolSlope = coef(bristolModel)[2] # Extract the slope from the model

# Plotting for Bristol
ggplot(bristolAnalysis, aes(x = `Average.download.speed..Mbit.s.`, y = Drug_Offense_Rate)) +
  geom_point(color = "blue", size = 2) + # Plot data points
  geom_abline(intercept = bristolIntercept, slope = bristolSlope, color = "red") + # Add regression line
  labs(
    title = "Impact of Broadband Speed on Drug Offense Rates in Bristol (2022)",
    x = "Average Download Speed (Mbit/s)",
    y = "Drug Offense Rate (per 10,000)"
  ) +
  theme_minimal() # Apply minimal theme to the plot

# Linear Modeling for Cornwall
cornwallAnalysis = inner_join(
  cornwallBroadband, 
  cornwallDrugRates,
  by = c("postcode_space" = "postcode")) %>% # Join broadband and crime rate data on postcode
  select(`Average.download.speed..Mbit.s.`, Drug_Offense_Rate) # Select columns for analysis

# Calculate correlation and fit linear model for Cornwall
correlation_cornwall = cornwallAnalysis %>% 
  summarise(correlation = cor(Drug_Offense_Rate, `Average.download.speed..Mbit.s.`)) # Compute correlation between broadband speed and drug offense rate
print(correlation_cornwall) # Print the correlation value

cornwallModel = lm(Drug_Offense_Rate ~ `Average.download.speed..Mbit.s.`, data = cornwallAnalysis) # Fit a linear model
summary(cornwallModel) # Print the summary of the linear model

cornwallIntercept = coef(cornwallModel)[1] # Extract the intercept from the model
cornwallSlope = coef(cornwallModel)[2] # Extract the slope from the model

# Plotting for Cornwall
ggplot(cornwallAnalysis, aes(x = `Average.download.speed..Mbit.s.`, y = Drug_Offense_Rate)) +
  geom_point(color = "blue", size = 2) + # Plot data points
  geom_abline(intercept = cornwallIntercept, slope = cornwallSlope, color = "red") + # Add regression line
  labs(
    title = "Impact of Broadband Speed on Drug Offense Rates in Cornwall (2022)",
    x = "Average Download Speed (Mbit/s)",
    y = "Drug Offense Rate (per 10,000)"
  ) +
  theme_minimal() # Apply minimal theme to the plot
