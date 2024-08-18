# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(plotly)     # For creating interactive plots

# Load cleaned datasets
bristolData = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\bristol_schools_data.csv")  
cornwallData = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\cornwall_schools_data.csv")  

# ---------------------------------------------------------

# Average Attainment 8 Scores for 2021-2022 across Both Counties (Boxplot)

# Data Preparation
combinedData = bind_rows(  # Combine data from both counties
  bristolData %>%
    filter(Year == 2022 & !ATT8SCR %in% c("SUPP", "NE")) %>%  
    # Filter Bristol data for the year 2022, excluding suppressed or not entered scores
    mutate(ATT8SCR = as.numeric(ATT8SCR), County = "Bristol"),  
    # Convert ATT8SCR to numeric and add a County column
  cornwallData %>%
    filter(Year == 2022 & !ATT8SCR %in% c("SUPP", "NE")) %>%  
    # Filter Cornwall data for the year 2022, excluding suppressed or not entered scores
    mutate(ATT8SCR = as.numeric(ATT8SCR), County = "Cornwall")) %>%  
    # Convert ATT8SCR to numeric and add a County column
  group_by(SchName, County) %>%  
  # Group data by School Name and County
  summarise(average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE))  
  # Calculate the average ATT8SCR for each school, ignoring NA values

# Create Boxplot
boxplot2022 = ggplot(combinedData, aes(x = County, y = average_ATT8SCR)) +  
# Set up the boxplot with County on x-axis and average_ATT8SCR on y-axis
  geom_boxplot() +  # Add boxplot geom
  labs(title = "Average Attainment 8 score in the year 2021-2022",  # Add title
       x = "County",  # Label x-axis
       y = "Average Attainment 8 Score") +  # Label y-axis
  theme_minimal()  # Use minimal theme for plot

# Interactive Plot
ggplotly(boxplot2022, tooltip = "text")  
# Convert ggplot to an interactive plotly plot

# ---------------------------------------------------------

# Line Chart: Average Attainment 8 Scores for Bristol Schools (2021-2022)

bristolLineChart = bristolData %>%
  filter(Year == 2022 & !ATT8SCR %in% c("SUPP", "NE")) %>%  
  # Filter Bristol data for the year 2022, excluding suppressed or not entered scores
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%  # Convert ATT8SCR to numeric
  group_by(SchName) %>%  # Group data by School Name
  summarise(average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE)) %>%  
  # Calculate the average ATT8SCR for each school, ignoring NA values
  ggplot(aes(x = reorder(SchName, average_ATT8SCR), y = average_ATT8SCR, group = 1)) +  
  # Set up the line chart with reordered School Name on x-axis and average_ATT8SCR on y-axis
  geom_line(color = "blue") +  # Add line geom with blue color
  geom_point(size = 3, color = "red") +  # Add points on the line with red color and size 3
  labs(title = "Average Attainment 8 Scores for Bristol Schools in 2021-2022",  # Add title
       x = "School Name",  # Label x-axis
       y = "Average Attainment 8 Score") +  # Label y-axis
  theme_minimal() +  # Use minimal theme for plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  
  # Rotate x-axis text labels for better readability
        plot.title = element_text(size = 14))  # Set title text size

# Interactive Line Chart
ggplotly(bristolLineChart, tooltip = "text")  
# Convert ggplot to an interactive plotly plot

# ---------------------------------------------------------

# Line Chart: Average Attainment 8 Scores for Cornwall Schools (2021-2022)

cornwallLineChart = cornwallData %>%
  filter(Year == 2022 & !ATT8SCR %in% c("SUPP", "NE")) %>%  
  # Filter Cornwall data for the year 2022, excluding suppressed or not entered scores
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%  
  # Convert ATT8SCR to numeric
  group_by(SchName) %>%  
  # Group data by School Name
  summarise(average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE)) %>%  
  # Calculate the average ATT8SCR for each school, ignoring NA values
  ggplot(aes(x = reorder(SchName, average_ATT8SCR), y = average_ATT8SCR, group = 1)) +  
  # Set up the line chart with reordered School Name on x-axis and average_ATT8SCR on y-axis
  geom_line(color = "palegreen4") +  
  # Add line geom with pale green color
  geom_point(size = 3, color = "hotpink4") +  
  # Add points on the line with hot pink color and size 3
  labs(title = "Average Attainment 8 Scores for Cornwall Schools in 2021-2022",  # Add title
       x = "School Name",  # Label x-axis
       y = "Average Attainment 8 Score") +  # Label y-axis
  theme_light() +  # Use light theme for plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  
  # Rotate x-axis text labels for better readability
        plot.title = element_text(size = 14))  # Set title text size

# Interactive Line Chart
ggplotly(cornwallLineChart, tooltip = "text")  
# Convert ggplot to an interactive plotly plot
