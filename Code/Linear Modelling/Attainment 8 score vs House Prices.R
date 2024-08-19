library(tidyverse)  # Loads the 'tidyverse' suite of packages for data manipulation and visualization
library(lubridate)  # Loads 'lubridate' for working with dates

# Import housing data for Bristol and Cornwall
bristol_housing = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_bristol.csv")
cornwall_housing = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_cornwall.csv")

# Display column names for verification
colnames(bristol_housing)
colnames(cornwall_housing)

# Import schools data for Bristol and Cornwall
bristol_schools = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\bristol_schools_data.csv")  
cornwall_schools = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\cornwall_schools_data.csv")  

# Display column names for verification
colnames(bristol_schools)
colnames(cornwall_schools)

# Filter and compute average house prices for 2022 and 2023 by postcode
avg_bristol_prices = bristol_housing %>% 
  filter(year(ymd(Year)) == 2022 | year(ymd(Year)) == 2023) %>% 
  # Filters the Bristol housing data for the years 2022 and 2023
  group_by(Postcode) %>%
  # Groups the data by postcode
  summarise(Average_Price = mean(Housing_Price, na.rm = TRUE), .groups = 'drop')
  # Calculates the average housing price for each postcode and drops the grouping structure

avg_cornwall_prices = cornwall_housing %>% 
  filter(year(ymd(Year)) == 2022 | year(ymd(Year)) == 2023) %>% 
  # Filters the Cornwall housing data for the years 2022 and 2023
  group_by(Postcode) %>%
  # Groups the data by postcode
  summarise(Average_Price = mean(Housing_Price, na.rm = TRUE), .groups = 'drop')
  # Calculates the average housing price for each postcode and drops the grouping structure

# Filter and compute average Attainment 8 scores for 2022 and 2023 by postcode
avg_bristol_att8 = bristol_schools %>% 
  filter(Year == 2022 | Year == 2023) %>% 
  # Filters the Bristol schools data for the years 2022 and 2023
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  # Excludes records with unsupported or missing Attainment 8 scores
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  # Converts Attainment 8 scores to numeric values
  group_by(Postcode) %>%
  # Groups the data by postcode
  summarise(Average_ATT8 = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')
  # Calculates the average Attainment 8 score for each postcode and drops the grouping structure

avg_cornwall_att8 = cornwall_schools %>% 
  filter(Year == 2022 | Year == 2023) %>% 
  # Filters the Cornwall schools data for the years 2022 and 2023
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  # Excludes records with unsupported or missing Attainment 8 scores
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  # Converts Attainment 8 scores to numeric values
  group_by(Postcode) %>%
  # Groups the data by postcode
  summarise(Average_ATT8 = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')
  # Calculates the average Attainment 8 score for each postcode and drops the grouping structure

# Combine housing and school data for Bristol
bristol_combined = inner_join(
  avg_bristol_prices, 
  avg_bristol_att8, 
  by = c("Postcode" = "Postcode")
  # Merges the Bristol average house prices and Attainment 8 scores data frames by postcode
) %>% 
  select(Average_Price, Average_ATT8)
  # Selects only the columns for average price and average Attainment 8 score

# Calculate correlation coefficient for Bristol
correlation_bristol = bristol_combined %>% 
  summarise(Correlation = cor(Average_Price, Average_ATT8))
  # Computes the correlation coefficient between average house prices and average Attainment 8 scores for Bristol
print(correlation_bristol)
# Prints the correlation coefficient for Bristol

# Negative but weak relationship (-0.291)

# Create a linear model for Bristol
bristol_model = lm(Average_Price ~ Average_ATT8, data = bristol_combined)
# Fits a linear regression model to predict average house price based on average Attainment 8 score
summary(bristol_model)
# Displays a summary of the linear model, including coefficients and statistical significance

# Extract coefficients for the Bristol model
bristol_intercept = coef(bristol_model)[1]
# Extracts the intercept of the Bristol linear model
bristol_slope = coef(bristol_model)[2]
# Extracts the slope of the Bristol linear model

# Visualize the relationship for Bristol
ggplot(bristol_combined, aes(x = Average_ATT8, y = Average_Price / 1000)) +
  geom_point(color = "blue", size = 2) +
  # Plots the average Attainment 8 scores against the average house prices (in thousands) as points
  geom_abline(intercept = bristol_intercept / 1000, slope = bristol_slope / 1000, color = "red") +
  # Adds a regression line to the plot
  labs(
    title = "Impact of Attainment 8 Scores on House Prices in Bristol (2022)",
    x = "Average Attainment 8 Scores",
    y = "Average House Price (in thousands)"
  ) +
  theme_minimal()
  # Sets the plot labels and theme

# Combine housing and school data for Cornwall
cornwall_combined = inner_join(
  avg_cornwall_prices, 
  avg_cornwall_att8, 
  by = c("Postcode" = "Postcode")
  # Merges the Cornwall average house prices and Attainment 8 scores data frames by postcode
) %>% 
  select(Average_Price, Average_ATT8)
  # Selects only the columns for average price and average Attainment 8 score

# Calculate correlation coefficient for Cornwall
correlation_cornwall = cornwall_combined %>% 
  summarise(Correlation = cor(Average_Price, Average_ATT8))
  # Computes the correlation coefficient between average house prices and average Attainment 8 scores for Cornwall
print(correlation_cornwall)
# Prints the correlation coefficient for Cornwall

# Strong negative relationship (-1)

# Create a linear model for Cornwall
cornwall_model = lm(Average_Price ~ Average_ATT8, data = cornwall_combined)
# Fits a linear regression model to predict average house price based on average Attainment 8 score
summary(cornwall_model)
# Displays a summary of the linear model, including coefficients and statistical significance

# Extract coefficients for the Cornwall model
cornwall_intercept = coef(cornwall_model)[1]
# Extracts the intercept of the Cornwall linear model
cornwall_slope = coef(cornwall_model)[2]
# Extracts the slope of the Cornwall linear model

# Visualize the relationship for Cornwall
ggplot(cornwall_combined, aes(x = Average_ATT8, y = Average_Price / 1000)) +
  geom_point(color = "blue", size = 2) +
  # Plots the average Attainment 8 scores against the average house prices (in thousands) as points
  geom_abline(intercept = cornwall_intercept / 1000, slope = cornwall_slope / 1000, color = "red") +
  # Adds a regression line to the plot
  labs(
    title = "Impact of Attainment 8 Scores on House Prices in Cornwall (2022)",
    x = "Average Attainment 8 Scores",
    y = "Average House Price (in thousands)"
  ) +
  theme_minimal()
  # Sets the plot labels and theme
