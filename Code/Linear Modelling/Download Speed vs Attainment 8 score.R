# Load necessary libraries
library(tidyverse) # for data manipulation and visualization
library(lubridate) # for date and time manipulation

# Load school data for Bristol and Cornwall
bristolSchools = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\bristol_schools_data.csv")
cornwallSchools = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\cornwall_schools_data.csv")

# Load broadband speed data for Bristol and Cornwall
bristolBroadband = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\bristol_broadband_data.csv")
cornwallBroadband = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\cornwall_broadband_data.csv")

# Filter and clean school data for the year 2022
bristolSchools2022 = bristolSchools %>%
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>%  # Remove suppressed and unavailable scores
  mutate(ATT8SCR = as.numeric(ATT8SCR)) # Convert Attainment 8 scores to numeric

cornwallSchools2022 = cornwallSchools %>%
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Merge school data with broadband speed data for Bristol
bristolAnalysis = inner_join(
  bristolSchools2022, 
  bristolBroadband,
  by = c("Postcode" = "postcode_space")
) %>% 
  select(ATT8SCR, `Average.download.speed..Mbit.s.`) # Select relevant columns

# Calculate correlation coefficient for Bristol
correlation_bristol = bristolAnalysis %>% 
  summarise(corCoeff = cor(ATT8SCR, `Average.download.speed..Mbit.s.`)) # Calculate correlation
print(correlation_bristol)

# Linear modeling for Bristol
bristolModel = lm(ATT8SCR ~ `Average.download.speed..Mbit.s.`, data = bristolAnalysis) # Create linear model
summary(bristolModel) # Summary of the model

# Extract coefficients for the linear model
bristolIntercept = coef(bristolModel)[1] # Intercept of the model
bristolSlope = coef(bristolModel)[2] # Slope of the model

# Plotting for Bristol
ggplot(bristolAnalysis, aes(x = `Average.download.speed..Mbit.s.`, y = ATT8SCR)) +
  geom_point(color = "blue", size = 2) + 
  geom_abline(intercept = bristolIntercept, slope = bristolSlope, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on Attainment 8 Score in Bristol Schools (2022)",
    x = "Average Download Speed",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

# Merge school data with broadband speed data for Cornwall
cornwallAnalysis = inner_join(
  cornwallSchools2022, 
  cornwallBroadband,
  by = c("Postcode" = "postcode_space")
) %>% 
  select(ATT8SCR, `Average.download.speed..Mbit.s.`)

# Calculate correlation coefficient for Cornwall
correlation_cornwall = cornwallAnalysis %>% 
  summarise(corCoeff = cor(ATT8SCR, `Average.download.speed..Mbit.s.`))
print(correlation_cornwall)

# Linear modeling for Cornwall
cornwallModel = lm(ATT8SCR ~ `Average.download.speed..Mbit.s.`, data = cornwallAnalysis)
summary(cornwallModel)

# Extract coefficients for the linear model
cornwallIntercept = coef(cornwallModel)[1]
cornwallSlope = coef(cornwallModel)[2]

# Plotting for Cornwall
ggplot(cornwallAnalysis, aes(x = `Average.download.speed..Mbit.s.`, y = ATT8SCR)) +
  geom_point(color = "blue", size = 2) + 
  geom_abline(intercept = cornwallIntercept, slope = cornwallSlope, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on Attainment 8 Score in Cornwall Schools (2022)",
    x = "Average Download Speed",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))
