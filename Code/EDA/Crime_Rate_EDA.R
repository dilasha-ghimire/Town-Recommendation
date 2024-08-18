library(tidyverse)  # Loads the tidyverse package, which includes ggplot2, dplyr, tidyr, and other packages for data manipulation and visualization.
library(plotly)     # Loads the plotly package for creating interactive plots.
library(lubridate)  # Loads the lubridate package for working with dates and times.
library(fmsb)       # For radar charts


# Load the cleaned data
bristol_crime = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\bristol-crime-data.csv")
cornwall_crime = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\cornwall-crime-data.csv")
popn_2011 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Crime Rate Data\\Population2011.csv")
# Reads the CSV files for Bristol and Cornwall crime data and population data for 2011 into R data frames.

# To convert the Population2011 csv data to Population 2023 
popn_2023 = popn_2011 %>% 
  mutate(Population = Population * 1.00561255390388033)
View(popn_2023)
# Converts the 2011 population data to the estimated 2023 population using a growth rate and stores it in a new data frame `popn_2023`.

# Conversion of population of 2023 to 2022
yrs_conversion = 2023-2011
ann_gr_rate = 1.00561255390388033 ^ (1/yrs_conversion)
popn_2022 = popn_2023 %>% 
  mutate(Population = Population / ann_gr_rate)
# Calculates the annual growth rate needed to convert the 2023 population estimate back to 2022 and creates a new data frame `popn_2022` with the 2022 population estimates.

# Remove the last digit after the space in the Postcode column
popn_2023 <- popn_2023 %>%
  mutate(Postcode = str_extract(Postcode, "^\\S+")) 

# Remove the last digit after the space in the Postcode column
popn_2022 <- popn_2022 %>%
  mutate(Postcode = str_extract(Postcode, "^\\S+")) 

# Convert Year column to Date type and extract Postcode
bristol_crime = bristol_crime %>%
  mutate(Postcode = str_extract(postcode, "^\\S+")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
# Processes Bristol crime data by extracting the postcode and converting the Year column to a date format (assuming all data is for January of each year).

cornwall_crime = cornwall_crime %>%
  mutate(Postcode = str_extract(postcode, "^\\S+")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))
# Similar processing for Cornwall crime data, ensuring consistency in date and postcode formats.

# Combine the datasets
combined_crime = bind_rows(bristol_crime, cornwall_crime)
# Merges the Bristol and Cornwall crime datasets into one combined dataset for analysis.

# Load the town dataset
town_dataset <- read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\town_dataset.csv")

# Select both Short_Postcode and Town_City columns from town_dataset
town_postcodes <- town_dataset %>%
  select(Short_Postcode, Town_City)

# Perform a left join with combined_crime on Postcode
combined_crime_with_town <- combined_crime %>%
  left_join(town_postcodes, by = c("Postcode" = "Short_Postcode"))

# View the resulting dataset
View(combined_crime_with_town)


# -----------------------------------------------------


# Drug offence rate in both counties towns or districts in the year 2022 (Boxplot)


# Filter for drug offences in 2022 and join with population data
drug_offences_2022 = combined_crime %>%
  filter(year(Year) == 2022) %>%
  left_join(popn_2022, by = "Postcode") %>%
  filter(Crime_type == "Drugs") %>%
  filter(city == "Bristol, City of" | city == "Cornwall") %>%
  filter(!is.na(Population)) %>%
  # Summarize data for box plot
  group_by(Postcode, city) %>%
  summarize(
    drug_offences = n(),
    population = first(Population)
  ) %>%
  mutate(drug_offence_rate = drug_offences / population * 10000)
# Filters the combined crime data to include only drug offences from 2022, joins with the population data for 2022, and summarizes the data to calculate drug offence rates per 10,000 people for each postcode and city.

# Create the box plot
drug_offence_box_plot = ggplot(drug_offences_2022, aes(x = city, y = drug_offence_rate)) +
  geom_boxplot() +
  labs(
    title = "Drug Offence Rates per 10,000 People in 2022",
    x = "County",
    y = "Drug Offence Rate per 10,000 People"
  ) +
  coord_cartesian(ylim = c(0, 60)) +
  theme_minimal() +
  scale_x_discrete(labels = c("Bristol, City of" = "Bristol", "Cornwall" = "Cornwall")) +
  theme(plot.title = element_text(size = 14))
# Creates a box plot of the drug offence rates per 10,000 people, showing distribution for Bristol and Cornwall. The plot is customized with labels, title, and limits for the y-axis.

# Make the plot interactive with plotly
ggplotly(drug_offence_box_plot)
# Converts the static ggplot box plot into an interactive plot using the plotly package.


# -----------------------------------------------------


# Vehicle Crime Rate per 10000 people in the Specific month of your choice in year 2022 (Radar Chart)

# Filter for vehicle crime in a specific month (e.g., July) in 2022
vehicle_crime_month <- combined_crime_with_town %>%
  filter(Crime_type == "Vehicle crime" & year(Year) == 2022 & month(Year) == 7) %>%  # Select rows for vehicle crime in July 2022
  left_join(popn_2022, by = "Postcode") %>%  # Join with population data on Postcode
  filter(!is.na(Population)) %>%  # Remove rows with missing population data
  group_by(Town_City) %>%  # Group data by Town/City
  summarize(TotalVehicleOffences = n(),  # Count the number of vehicle crime offences
            Population = sum(Population),  # Sum the population for each Town/City
            VehicleCrimeRatePerTenThousand = (TotalVehicleOffences / Population) * 10000)  # Calculate the vehicle crime rate per 10,000 people

# Prepare data for radar chart
max_value <- max(vehicle_crime_month$VehicleCrimeRatePerTenThousand, na.rm = TRUE)  # Find the maximum value of vehicle crime rate
min_value <- min(vehicle_crime_month$VehicleCrimeRatePerTenThousand, na.rm = TRUE)  # Find the minimum value of vehicle crime rate

radar_data <- as.data.frame(rbind(
  rep(max_value, nrow(vehicle_crime_month)), # Create a row with the maximum value repeated for each Town/City (used for the maximum scale in the radar chart)
  rep(min_value, nrow(vehicle_crime_month)), # Create a row with the minimum value repeated for each Town/City (used for the minimum scale in the radar chart)
  vehicle_crime_month$VehicleCrimeRatePerTenThousand # Include the actual vehicle crime rate data for plotting
))

# Set row and column names
rownames(radar_data) <- c("Max", "Min", "Actual Data")  # Define row names for the radar chart data
colnames(radar_data) <- vehicle_crime_month$Town_City  # Define column names based on Town/City names

# Plot the radar chart
radarchart(radar_data,  # Plot the radar chart using the `radarchart` function from the `fmsb` package
           axistype = 1,  # Set the type of axis (1 indicates a standard radar chart)
           pcol = rainbow(ncol(radar_data)),  # Set the color of the lines (one color per Town/City)
           pfcol = scales::alpha(rainbow(ncol(radar_data)), 0.3),  # Set the fill color of the areas with some transparency
           plwd = 2,  # Set the line width
           title = "Vehicle Crime Rate per 10,000 People in July 2022 by Town/City",  # Set the title of the radar chart
           cglcol = "grey",  # Set the color of the grid lines
           cglty = 1,  # Set the type of the grid lines (1 indicates solid lines)
           axislabcol = "black",  # Set the color of the axis labels
           caxislabels = seq(0, max_value, by = 25),  # Define the labels for the axis at intervals of 25
           cglwd = 0.8,  # Set the width of the grid lines
           vlcex = 0.7)  # Reduce the size of the axis labels text for better readability


# -----------------------------------------------------


# Robbery crime rate per 10000 people in the specific month of your choice in year 2022 (Pie Chart)

# Filter for robbery crimes in December 2022
robbery_crime_month <- combined_crime_with_town %>%
  filter(Crime_type == "Burglary" & year(Year) == 2022 & month(Year) == 12) %>%  # Select records where Crime_type is "Burglary", Year is 2022, and month is December
  left_join(popn_2022, by = "Postcode") %>%  # Join with the population dataset based on Postcode
  filter(!is.na(Population), !is.na(Town_City)) %>%  # Remove records where Population or Town_City is missing
  group_by(Town_City) %>%  # Group by Town/City to perform aggregation
  summarize(
    TotalRobberyOffences = n(),  # Count the number of robbery offences in each Town/City
    Population = sum(Population),  # Sum the population for each Town/City
    RobberyCrimeRatePerTenThousand = (TotalRobberyOffences / Population) * 10000  # Calculate the robbery crime rate per 10,000 people
  )

# Create a pie chart
robbery_pie_chart <- robbery_crime_month %>%
  mutate(Percentage = (RobberyCrimeRatePerTenThousand / sum(RobberyCrimeRatePerTenThousand)) * 100,  # Calculate percentage contribution of each Town/City's crime rate
         Label = paste0(round(Percentage, 1), "%")) %>%  # Create labels with rounded percentage values
  ggplot(aes(x = "", y = RobberyCrimeRatePerTenThousand, fill = Town_City)) +  # Set up the ggplot aesthetics for a pie chart
  geom_bar(width = 1, stat = "identity") +  # Create a bar plot with bars having width 1 (essentially making it a pie chart)
  coord_polar("y", start = 0) +  # Convert bar plot to a pie chart
  theme_void() +  # Remove axes and background for a cleaner pie chart appearance
  labs(
    title = "Robbery Crime Rate per 10,000 People in December 2022 by Town/City",  # Set the title of the pie chart
    fill = "Town/City"  # Set the legend title
  ) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +  # Add labels with percentages to the pie chart
  theme(plot.title = element_text(size = 14))  # Set the title font size

# Display the pie chart
print(robbery_pie_chart)


# -----------------------------------------------------


# Drug offense rate per 10000 people in both counties (Line Chart) 
# should be shown in one single image


# Filter for drug offences in the year 2022
# Join with population data to get the population for each postcode
# Filter for drug crime types and cities of interest
# Exclude rows with missing population data
# Group by city and year to summarize the data
# Calculate the drug offence rate per 10,000 people
drug_offences_2022 = combined_crime %>%
  filter(year(Year) == 2022) %>% # Filter for data from 2022
  left_join(popn_2022, by = "Postcode") %>% # Join with population data
  filter(Crime_type == "Drugs") %>% # Filter for drug crimes
  filter(city == "Bristol, City of" | city == "Cornwall") %>% # Filter for Bristol and Cornwall
  filter(!is.na(Population)) %>% # Remove rows with missing population data
  mutate(city = recode(city, 
                       "Bristol, City of" = "Bristol", 
                       "Cornwall" = "Cornwall")) %>% # Recode city names
  group_by(city, Year) %>% # Group by city and year
  summarise(drug_offences = n(), # Count the number of drug offences
            population = first(Population)) %>% # Get the population (first occurrence for simplicity)
  mutate(drug_offence_rate = drug_offences / population * 10000) # Calculate drug offence rate per 10,000 people

# Create a line chart to visualize the drug offence rates
line_chart = ggplot(drug_offences_2022, aes(x = Year, y = drug_offence_rate, color = city)) +
  geom_line() + # Add lines for each city
  geom_point() + # Add points for each data point
  labs(title = "Drug Offence Rates per 10,000 People in Bristol and Cornwall (2022)",
       x = "Date", # Label for the x-axis
       y = "Drug Offence Rate per 10,000 People") + # Label for the y-axis
  theme_minimal() + # Apply a minimal theme
  scale_color_manual(values = c("Bristol" = "blue", "Cornwall" = "red"))

# Print the line chart
print(line_chart)
