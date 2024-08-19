# Step 1: Set working directory
# This ensures that any relative file paths used in your 
# script will be relative to this directory.

setwd("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Code\\Cleaning")


# Step 2: Load necessary libraries

#install.packages("dplyr")
library(dplyr)
library(readr)
library(tidyr)


# Step 3: Read the datasets

data_2020 = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\House Pricing Data\\House Pricing-2020.csv", header=FALSE)
data_2021 = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\House Pricing Data\\House Pricing-2021.csv", header=FALSE)
data_2022 = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\House Pricing Data\\House Pricing-2022.csv", header=FALSE)
data_2023 = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\House Pricing Data\\House Pricing-2023.csv", header=FALSE)


# Step 4: Combine the datasets
# `bind_rows` is used to stack the datasets on top of each other.

all_data = bind_rows(data_2020, data_2021, data_2022, data_2023)


# Step 5: Filter for Cornwall, select specific columns, 
# rename them, remove duplicates, and format Year column

# `filter()` is used to keep only the rows where the city 
# (assumed to be in the 14th column, V14) is "CORNWALL"

# `select()` renames columns (V1, V2, V3, V4, V5, V12, V13, V14) to more descriptive names 
# (ID, Housing_Price, Year, Postcode, Type, Town_City, District, County)

# `mutate()` formats the Year column to remove the time and convert it to a Date format

# Removes duplicate rows using the `distinct()` function.

# Step 6: Save the cleaned data
# `write.csv` writes the filtered data to a new CSV file.

all_data %>%
  filter(V14 == "CORNWALL") %>% 
  # Filter rows where county is Cornwall
  select(ID = V1, Housing_Price = V2, Year = V3, Postcode = V4, Type = V5, Town_City = V12, District = V13, County = V14) %>%
  # Select and rename specific columns
  mutate(Year = as.Date(Year, format = "%Y-%m-%d"), 
         Short_Postcode = sub(" .*", "", Postcode)) %>%
  # Format Year column to remove time and convert to Date, and create Short_Postcode column
  distinct() %>%
  # Remove duplicate rows
  mutate(Postcode = if_else(Postcode == "", "Not available", Postcode),
         Short_Postcode = if_else(Short_Postcode == "", "Not available", Short_Postcode)) %>%
  # Replace empty strings with "Not available" for Postcode and Short_Postcode only
  write_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Housing Data\\house_pricing_cornwall.csv")
# Save the cleaned data


# Step 7: Load the cleaned data

house_pricing_data_cornwall = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_cornwall.csv")
View(house_pricing_data_cornwall)

# -----------------------------------------------------------------------

# Repeating Step 6 and 7 for Bristol

all_data %>%
  filter(V14 == "CITY OF BRISTOL") %>% 
  # Filter rows where county is Bristol
  select(ID = V1, Housing_Price = V2, Year = V3, Postcode = V4, Type = V5, Town_City = V12, District = V13, County = V14) %>%
  # Select and rename specific columns
  mutate(Year = as.Date(Year, format = "%Y-%m-%d"), 
         Short_Postcode = sub(" .*", "", Postcode)) %>%
  # Format Year column to remove time and convert to Date, and create Short_Postcode column
  distinct() %>%
  # Remove duplicate rows
  filter(!(County == "CITY OF BRISTOL" & Town_City %in% c("LONDON", "WELLS", "WESTON-SUPER-MARE"))) %>%
  # Filter out incorrect cities under CITY OF BRISTOL county
  mutate(Postcode = if_else(Postcode == "", "Not available", Postcode),
         Short_Postcode = if_else(Short_Postcode == "", "Not available", Short_Postcode)) %>%
  # Replace empty strings with "Not available" for Postcode and Short_Postcode only
  write_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Housing Data\\house_pricing_bristol.csv")
# Save the cleaned data

house_pricing_data_bristol = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_bristol.csv")
View(house_pricing_data_bristol)

# -----------------------------------------------------------------------

# Cornwall
cornwall_data = all_data %>%
  filter(V14 == "CORNWALL") %>%
  select(ID = V1, Housing_Price = V2, Year = V3, Postcode = V4, Type = V5, Town_City = V12, District = V13, County = V14) %>%
  mutate(Year = as.Date(Year, format = "%Y-%m-%d"), 
         Short_Postcode = sub(" .*", "", Postcode)) %>%
  distinct() %>%
  mutate(Postcode = if_else(Postcode == "", "Not available", Postcode),
         Short_Postcode = if_else(Short_Postcode == "", "Not available", Short_Postcode))

# Bristol
bristol_data = all_data %>%
  filter(V14 == "CITY OF BRISTOL") %>%
  select(ID = V1, Housing_Price = V2, Year = V3, Postcode = V4, Type = V5, Town_City = V12, District = V13, County = V14) %>%
  mutate(Year = as.Date(Year, format = "%Y-%m-%d"), 
         Short_Postcode = sub(" .*", "", Postcode)) %>%
  distinct() %>%
  filter(!(County == "CITY OF BRISTOL" & Town_City %in% c("LONDON", "WELLS", "WESTON-SUPER-MARE"))) %>%
  mutate(Postcode = if_else(Postcode == "", "Not available", Postcode),
         Short_Postcode = if_else(Short_Postcode == "", "Not available", Short_Postcode))

# Step 6: Combine cleaned data for Cornwall and Bristol
combined_data = bind_rows(cornwall_data, bristol_data)  

# Step 7: Save the combined data to a new CSV file
write_csv(combined_data, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_combined.csv")

# Step 8: Load and view the combined data
house_pricing_data_combined = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_combined.csv")
View(house_pricing_data_combined)

# -----------------------------

house_pricing = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\house_pricing_combined.csv")

town_dataset = house_pricing[, c("Short_Postcode", "Town_City", "County")]

town_dataset = town_dataset %>%
  filter(Short_Postcode != "Not available")

town_dataset = distinct(town_dataset)

n_distinct(town_dataset$Town_City)

write_csv(town_dataset, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned House Pricing Data\\town_dataset.csv")

