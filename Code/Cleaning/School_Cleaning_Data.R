library(tidyverse)  # Load the tidyverse package, which includes dplyr, ggplot2, readr, and other useful packages
library(readr)      # Load the readr package for reading and writing CSV files
library(dplyr)      # Load the dplyr package for data manipulation
library(DescTools)  # Load the DescTools package for descriptive statistics and data analysis


# BRISTOL SCHOOLS 2021-2022

# Load the KS4 final data for Bristol from CSV
bristolKs4_21_22 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Schools Data\\bristol_21-22\\2021-2022\\801_ks4final.csv")

# Add the year column with value 2022 and select relevant columns
bristolKs4_21_22 = bristolKs4_21_22 %>%
  mutate(Year = "2022") %>%   # Add Year column with value 2022
  select(PCODE, Year, SCHNAME, ATT8SCR)  # Select relevant columns for further processing

# Remove rows where school names or assessment scores are missing
bristolKs4_21_22 = bristolKs4_21_22 %>%
  drop_na(SCHNAME, ATT8SCR) %>%  # Drop rows with missing values in SCHNAME or ATT8SCR
  rename(Postcode = PCODE, SchName = SCHNAME)  # Rename columns to match desired names

# Load the school information data for Bristol from CSV
bristolSchoolInfo_21_22 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Schools Data\\bristol_21-22\\2021-2022\\801_school_information.csv")

# Consolidate school level columns into one and select relevant columns
bristolSchoolInfo_21_22 = bristolSchoolInfo_21_22 %>%
  mutate(SchLevel = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16'
  )) %>%  # Create SchLevel based on ISPRIMARY, ISSECONDARY, ISPOST16 columns
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SchLevel, OFSTEDRATING) %>%  # Select relevant columns
  rename(Postcode = POSTCODE, SchName = SCHNAME, Town = TOWN, SchoolType = SCHOOLTYPE)  # Rename columns to match desired names

# Combine KS4 final data with school information data using inner join
bristolSchools_21_22 = inner_join(bristolKs4_21_22, bristolSchoolInfo_21_22, by = c("Postcode", "SchName"))

# Handle missing TOWN values and add County information
bristolSchools_21_22 = bristolSchools_21_22 %>%
  mutate(Town = if_else(is.na(Town), "Bristol", Town),  # Fill missing TOWN values with "Bristol"
         County = "City of Bristol") %>%  # Add County information
  filter(Town == "Bristol")  # Filter rows to include only those with Town as "Bristol"


# BRISTOL SCHOOLS 2022-2023

# Load the KS4 final data for Bristol from CSV
bristolKs4_22_23 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Schools Data\\bristol_22-23\\2022-2023\\801_ks4final.csv")

# Add the year column with value 2023 and select relevant columns
bristolKs4_22_23 = bristolKs4_22_23 %>%
  mutate(Year = "2023") %>%   # Add Year column with value 2023
  select(PCODE, Year, SCHNAME, ATT8SCR)  # Select relevant columns for further processing

# Remove rows where school names or assessment scores are missing
bristolKs4_22_23 = bristolKs4_22_23 %>%
  drop_na(SCHNAME, ATT8SCR) %>%  # Drop rows with missing values in SCHNAME or ATT8SCR
  rename(Postcode = PCODE, SchName = SCHNAME)  # Rename columns to match desired names

# Load the school information data for Bristol from CSV
bristolSchoolInfo_22_23 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Schools Data\\bristol_22-23\\2022-2023\\801_school_information.csv")

# Consolidate school level columns into one and select relevant columns
bristolSchoolInfo_22_23 = bristolSchoolInfo_22_23 %>%
  mutate(SchLevel = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16'
  )) %>%  # Create SchLevel based on ISPRIMARY, ISSECONDARY, ISPOST16 columns
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SchLevel, OFSTEDRATING) %>%  # Select relevant columns
  rename(Postcode = POSTCODE, SchName = SCHNAME, Town = TOWN, SchoolType = SCHOOLTYPE)  # Rename columns to match desired names

# Combine KS4 final data with school information data using inner join
bristolSchools_22_23 = inner_join(bristolKs4_22_23, bristolSchoolInfo_22_23, by = c("Postcode", "SchName"))

# Handle missing TOWN values and add County information
bristolSchools_22_23 = bristolSchools_22_23 %>%
  mutate(Town = if_else(is.na(Town), "Bristol", Town),  # Fill missing TOWN values with "Bristol"
         County = "City of Bristol") %>%  # Add County information
  filter(Town == "Bristol")  # Filter rows to include only those with Town as "Bristol"


# CORNWALL SCHOOLS 2021-2022

# Load the KS4 final data for Cornwall from CSV
cornwallKs4_21_22 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Schools Data\\cornwall_21-22\\2021-2022\\908_ks4final.csv")

# Add the year column with value 2022 and select relevant columns
cornwallKs4_21_22 = cornwallKs4_21_22 %>%
  mutate(Year = "2022") %>%   # Add Year column with value 2022
  select(PCODE, Year, SCHNAME, ATT8SCR)  # Select relevant columns for further processing

# Remove rows where school names or assessment scores are missing
cornwallKs4_21_22 = cornwallKs4_21_22 %>%
  drop_na(SCHNAME, ATT8SCR) %>%  # Drop rows with missing values in SCHNAME or ATT8SCR
  rename(Postcode = PCODE, SchName = SCHNAME)  # Rename columns to match desired names

# Load the school information data for Cornwall from CSV
cornwallSchoolInfo_21_22 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Schools Data\\cornwall_21-22\\2021-2022\\908_school_information.csv")

# Consolidate school level columns into one and select relevant columns
cornwallSchoolInfo_21_22 = cornwallSchoolInfo_21_22 %>%
  mutate(SchLevel = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16'
  )) %>%  # Create SchLevel based on ISPRIMARY, ISSECONDARY, ISPOST16 columns
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SchLevel, OFSTEDRATING) %>%  # Select relevant columns
  rename(Postcode = POSTCODE, SchName = SCHNAME, Town = TOWN, SchoolType = SCHOOLTYPE)  # Rename columns to match desired names

# Combine KS4 final data with school information data using inner join
cornwallSchools_21_22 = inner_join(cornwallKs4_21_22, cornwallSchoolInfo_21_22, by = c("Postcode", "SchName"))

# Handle missing TOWN values and add County information
cornwallSchools_21_22 = cornwallSchools_21_22 %>%
  mutate(Town = if_else(is.na(Town) & SchName == "The Lowen School", "Gunnislake", Town),  # Specific handling for known missing town names
         Town = if_else(is.na(Town) & SchName == "Red Moor School", "Lanlivery", Town),
         County = "Cornwall")  # Add County information


# CORNWALL SCHOOLS 2022-2023

# Load the KS4 final data for Cornwall from CSV
cornwallKs4_22_23 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Schools Data\\cornwall_22-23\\2022-2023\\908_ks4final.csv")

# Add the year column with value 2023 and select relevant columns
cornwallKs4_22_23 = cornwallKs4_22_23 %>%
  mutate(Year = "2023") %>%   # Add Year column with value 2023
  select(PCODE, Year, SCHNAME, ATT8SCR)  # Select relevant columns for further processing

# Remove rows where school names or assessment scores are missing
cornwallKs4_22_23 = cornwallKs4_22_23 %>%
  drop_na(SCHNAME, ATT8SCR) %>%  # Drop rows with missing values in SCHNAME or ATT8SCR
  rename(Postcode = PCODE, SchName = SCHNAME)  # Rename columns to match desired names

# Load the school information data for Cornwall from CSV
cornwallSchoolInfo_22_23 = read_csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Schools Data\\cornwall_22-23\\2022-2023\\908_school_information.csv")

# Consolidate school level columns into one and select relevant columns
cornwallSchoolInfo_22_23 = cornwallSchoolInfo_22_23 %>%
  mutate(SchLevel = case_when(
    ISPRIMARY == 1 ~ 'Primary',
    ISSECONDARY == 1 ~ 'Secondary',
    ISPOST16 == 1 ~ 'Post-16'
  )) %>%  # Create SchLevel based on ISPRIMARY, ISSECONDARY, ISPOST16 columns
  select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SchLevel, OFSTEDRATING) %>%  # Select relevant columns
  rename(Postcode = POSTCODE, SchName = SCHNAME, Town = TOWN, SchoolType = SCHOOLTYPE)  # Rename columns to match desired names

# Combine KS4 final data with school information data using inner join
cornwallSchools_22_23 = inner_join(cornwallKs4_22_23, cornwallSchoolInfo_22_23, by = c("Postcode", "SchName"))

# Handle missing TOWN values and add County information
cornwallSchools_22_23 = cornwallSchools_22_23 %>%
  mutate(Town = if_else(is.na(Town) & SchName == "The Lowen School", "Gunnislake", Town),  # Specific handling for known missing town names
         Town = if_else(is.na(Town) & SchName == "Red Moor School", "Lanlivery", Town),
         County = "Cornwall")  # Add County information


# -----------------------------------------------------


# Combine Bristol data
combinedBristol = bind_rows(bristolSchools_21_22, bristolSchools_22_23)

# Combine Cornwall data
combinedCornwall = bind_rows(cornwallSchools_21_22, cornwallSchools_22_23)

# Calculate mode of OFSTEDRATING for Bristol
mode_ofstedrating = Mode(na.omit(combinedBristol$OFSTEDRATING))

# Clean Bristol Schools Data
BristolSchoolsData = combinedBristol %>% 
  mutate(County = "City of Bristol") %>% 
  mutate(OFSTEDRATING = if_else(is.na(OFSTEDRATING), mode_ofstedrating, OFSTEDRATING))

# Calculate mode of OFSTEDRATING for Cornwall
mode_ofstedrating_cornwall = Mode(na.omit(combinedCornwall$OFSTEDRATING))

# Clean Cornwall Schools Data
CornwallSchoolsData = combinedCornwall %>% 
  mutate(County = "Cornwall") %>% 
  mutate(OFSTEDRATING = if_else(is.na(OFSTEDRATING), mode_ofstedrating_cornwall, OFSTEDRATING))

# Export cleaned Bristol data
write_csv(BristolSchoolsData, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\bristol_schools_data.csv")

# Optionally view the dataset in RStudio
View(BristolSchoolsData)

# Export cleaned Cornwall data
write_csv(CornwallSchoolsData, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Schools Data\\cornwall_schools_data.csv")

# Optionally view the dataset in RStudio
View(CornwallSchoolsData)