library(tidyverse)  # Load the tidyverse package for data manipulation and visualization
library(plotly)     # Load the plotly package for interactive plots

# Load your datasets from the specified file paths
bristolBroadbandSpeed = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\bristol_broadband_data.csv")
cornwallBroadbandSpeed = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\cornwall_broadband_data.csv")
combinedBroadbandData = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\combined_broadband_data.csv")

# ---------------------------------------------------------

# Average Download Speeds in Both Counties (Boxplot)

# Create a dictionary to map postcode areas to their respective counties
postcode_to_county = c("BS" = "CITY OF BRISTOL", "PL" = "CORNWALL", "TR" = "CORNWALL")

# Add a new column 'County' to the combined dataset using the dictionary
combinedBroadbandData = combinedBroadbandData %>%
  mutate(County = postcode_to_county[`postcode.area`])  # Map the 'postcode.area' to 'County'

# Create a boxplot to visualize the average download speeds in Bristol and Cornwall
boxplot_avg_speed = combinedBroadbandData %>%
  ggplot(aes(x = County, y = `Average.download.speed..Mbit.s.`)) +  # Define the aesthetics for the plot
  geom_boxplot() +  # Add a boxplot layer to the plot
  labs(
    title = "Average Download Speed in Bristol and Cornwall",  # Title of the plot
    x = "County",  # Label for the x-axis
    y = "Average Download Speed (Mbit/s)"  # Label for the y-axis
  ) +
  theme_minimal() +  # Apply a minimal theme to the plot for a clean look
  coord_cartesian(ylim = c(1.4, 5.5))  # Set y-axis limits to focus on a specific range


# Convert the ggplot to an interactive plot using ggplotly
ggplotly(boxplot_avg_speed, tooltip = "text")  # Add tooltips to the interactive plot

# ---------------------------------------------------------

# Average and Maximum Download Speeds in Both Counties (Bar Chart) 
# Two Diagram (One for Bristol and another for Cornwall)

# Function to create bar chart for average and maximum download speeds
create_bar_chart = function(data, county_name, sample_size) {
  # Filter the dataset for the specified city
  data %>%
    filter(county == county_name) %>%
    # Group the data by LSOA area
    group_by(lsoa_area) %>%
    # Calculate the average and maximum download speeds for each LSOA area
    summarise(
      avg_speed = mean(`Average.download.speed..Mbit.s.`, na.rm = TRUE),
      max_speed = max(`Maximum.download.speed..Mbit.s.`, na.rm = TRUE)
    ) %>%
    # Take a random sample of the specified number of LSOA areas
    slice_sample(n = sample_size) %>%
    # Gather the columns avg_speed and max_speed into long format
    gather(key = "speed_type", value = "speed", avg_speed, max_speed) %>%
    # Create a ggplot object
    ggplot(aes(x = lsoa_area, y = speed, fill = speed_type)) +
    # Add a bar chart layer with dodge position for grouped bars
    geom_bar(stat = "identity", position = "dodge") +
    # Add labels and title to the plot
    labs(
      title = paste("Average and Maximum Download Speeds in LSOA Areas of", county_name),
      x = "LSOA Area",
      y = "Download Speed (Mbit/s)",
      fill = "Speed Type"
    ) +
    # Apply a minimal theme to the plot for a clean look
    theme_minimal() +
    # Adjust the x-axis text angle for better readability
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Create the bar chart for Bristol using the bristolBroadbandSpeed dataset
bristol_bar_chart = create_bar_chart(bristolBroadbandSpeed, "Bristol, City of", 30) +
  labs(title = "Average and Maximum Download Speeds in LSOA Areas of City of Bristol")

# Convert the ggplot object to an interactive plotly object
ggplotly(bristol_bar_chart, tooltip = "text")

# Create the bar chart for Cornwall using the cornwallBroadbandSpeed dataset
cornwall_bar_chart = create_bar_chart(cornwallBroadbandSpeed, "Cornwall", 30)
# Convert the ggplot object to an interactive plotly object
ggplotly(cornwall_bar_chart, tooltip = "text")
