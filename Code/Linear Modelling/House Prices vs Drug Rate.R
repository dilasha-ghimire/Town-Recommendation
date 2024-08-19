library(tidyverse) # Load the tidyverse package for data manipulation and visualization
library(lubridate) # Load the lubridate package for date handling

# Load house pricing data for Bristol and Cornwall
house_pricing_data_bristol = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_bristol.csv")
house_pricing_data_cornwall = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_cornwall.csv")

# Display column names to verify the structure of the datasets
colnames(house_pricing_data_bristol)
colnames(house_pricing_data_cornwall)

# Load crime data for Bristol and Cornwall
bristolCrimeData = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\bristol-crime-data.csv")
cornwallCrimeData = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\cornwall-crime-data.csv")

# Display column names to verify the structure of the datasets
colnames(bristolCrimeData)
colnames(cornwallCrimeData)

# Load population data from 2011
population_2011 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\Population2011.csv")

# Display column names to verify the structure of the dataset
colnames(population_2011)

# Convert 'Year' column to date format and extract the year for house pricing data
house_pricing_data_bristol = house_pricing_data_bristol %>%
  mutate(Year = year(ymd(Year)))  # Convert 'Year' to numeric year

house_pricing_data_cornwall = house_pricing_data_cornwall %>%
  mutate(Year = year(ymd(Year)))  # Convert 'Year' to numeric year
  
# Filter house pricing data for the year 2022
bristolHousePrice2022 = house_pricing_data_bristol %>% 
  filter(Year == 2022)
cornwallHousePrice2022 = house_pricing_data_cornwall %>% 
  filter(Year == 2022)

# Display the dimensions to verify the number of rows in the filtered datasets
dim(bristolHousePrice2022)
dim(cornwallHousePrice2022)

# Convert population of 2011 to that of 2023 using a growth rate
population_2023 = population_2011 %>% 
  mutate(Population = Population * 1.00561255390388033)

# Calculate the annual growth rate and convert population to 2022
years_between = 2023 - 2011
annual_growth_rate = 1.00561255390388033 ^ (1/years_between)
population_2022 = population_2023 %>% 
  mutate(Population = Population / annual_growth_rate)

# Preprocess Bristol crime data
bristolCrimeData = bristolCrimeData %>%
  mutate(Postcode = str_extract(postcode, "^\\S+ \\d")) %>% # Extract relevant part of postcode
  mutate(Year = ymd(paste0(Year, "-01"))) # Convert Year to date format

# Preprocess Cornwall crime data
cornwallCrimeData = cornwallCrimeData %>%
  mutate(Postcode = str_extract(postcode, "^\\S+ \\d")) %>% # Extract relevant part of postcode
  mutate(Year = ymd(paste0(Year, "-01"))) # Convert Year to date format

# Calculate drug offense rate per 10,000 population for Bristol in 2022
bs_drugOffenceRate_2022 = bristolCrimeData %>% 
  filter(year(Year) == 2022) %>% # Filter data for the year 2022
  left_join(population_2022, by = "Postcode") %>% # Join with population data
  filter(Crime_type == "Drugs") %>% # Filter for drug offenses
  filter(city == "Bristol, City of") %>% # Filter for city of Bristol
  filter(!is.na(Population)) %>% # Remove rows with missing population data
  group_by(postcode) %>%
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000) # Calculate rate per 10,000 population

# Calculate drug offense rate per 10,000 population for Cornwall in 2022
cw_drugOffenceRate_2022 = cornwallCrimeData %>% 
  filter(year(Year) == 2022) %>% # Filter data for the year 2022
  left_join(population_2022, by = "Postcode") %>% # Join with population data
  filter(Crime_type == "Drugs") %>% # Filter for drug offenses
  filter(city == "Cornwall") %>% # Filter for city of Cornwall
  filter(!is.na(Population)) %>% # Remove rows with missing population data
  group_by(postcode) %>%
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000) # Calculate rate per 10,000 population

# Merge house pricing data with drug offense rates for Bristol
bs_housePrice_drugRate_2022 = inner_join(
  bristolHousePrice2022, bs_drugOffenceRate_2022,
  by = c("Postcode" = "postcode")) %>% 
  select(Housing_Price, drug_offense_rate)

# Calculate correlation coefficient for Bristol
corCoeff_bs = bs_housePrice_drugRate_2022 %>% 
  summarise(corCoeff = cor(Housing_Price, drug_offense_rate))
print(corCoeff_bs)

# Negative but weak relationship (-0.0260)

# Fit a linear model for Bristol
bsModel_housePrice_drugRate = lm(Housing_Price ~ drug_offense_rate, data = bs_housePrice_drugRate_2022)
summary(bsModel_housePrice_drugRate)

# Extract coefficients for Bristol
bsIntercept_housePrice_drugRate = coef(bsModel_housePrice_drugRate)[1]
bsSlope_housePrice_drugRate = coef(bsModel_housePrice_drugRate)[2]

# Plot data and regression line for Bristol
ggplot(bs_housePrice_drugRate_2022, aes(x = drug_offense_rate, y = Housing_Price / 10000)) + 
  geom_point(color = "blue", size = 2) + 
  geom_abline(intercept = bsIntercept_housePrice_drugRate / 10000, slope = bsSlope_housePrice_drugRate / 10000, color = "red") + 
  labs(
    title = "Impact of Drug Offense on House Price in Bristol (2022)",
    x = "Drug Offense Rate (per 10,000 population)",
    y = "House Price (in thousands)"
  ) +
  theme_minimal()

# Merge house pricing data with drug offense rates for Cornwall
cw_housePrice_drugRate_2022 = inner_join(
  cornwallHousePrice2022, cw_drugOffenceRate_2022,
  by = c("Postcode" = "postcode")) %>% 
  select(Housing_Price, drug_offense_rate)

# Calculate correlation coefficient for Cornwall
corCoeff_cw = cw_housePrice_drugRate_2022 %>% 
  summarise(corCoeff = cor(Housing_Price, drug_offense_rate))
print(corCoeff_bs)

# Negative but weak relationship (-0.0260)

# Fit a linear model for Cornwall
cwModel_housePrice_drugRate = lm(Housing_Price ~ drug_offense_rate, data = cw_housePrice_drugRate_2022)
summary(cwModel_housePrice_drugRate)

# Extract coefficients for Cornwall
cwIntercept_housePrice_drugRate = coef(cwModel_housePrice_drugRate)[1]
cwSlope_housePrice_drugRate = coef(cwModel_housePrice_drugRate)[2]

# Plot data and regression line for Cornwall
ggplot(cw_housePrice_drugRate_2022, aes(x = drug_offense_rate, y = Housing_Price / 10000)) + 
  geom_point(color = "blue", size = 2) + 
  geom_abline(intercept = cwIntercept_housePrice_drugRate / 10000, slope = cwSlope_housePrice_drugRate / 10000, color = "red") + 
  labs(
    title = "Impact of Drug Offense on House Price in Cornwall (2022)",
    x = "Drug Offense Rate (per 10,000 population)",
    y = "House Price (in thousands)"
  ) +
  theme_minimal()
