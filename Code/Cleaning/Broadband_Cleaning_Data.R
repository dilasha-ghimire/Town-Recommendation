# Load necessary libraries
library(tidyverse)

# Load datasets from specified file paths
broadband_performance = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Broadband Speed Data\\Fixed postcode Perfomance.csv")
broadband_coverage = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Broadband Speed Data\\Fixed postcode Coverage.csv")
postcode_to_lsoa = read.csv("C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Obtain Data\\Broadband Speed Data\\Postcode to LSOA.csv")

# Process and transform the broadband_performance dataset
broadband_performance = broadband_performance %>%
  # Handle missing values:
  # Replace NA values in numeric columns with either 0 (if >90% values are missing) 
  # or the median of the column (for columns with less than 90% missing values)
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 
                                            ifelse(sum(is.na(.)) > 0.9 * length(.), 0, median(., na.rm = TRUE)), 
                                            .))) %>%
  # Apply log transformation to numeric columns:
  # Adding 1 before log transformation to handle zeros and avoid log(0) which is undefined
  mutate(across(where(is.numeric), ~ log(. + 1))) 

# Process and transform the broadband_coverage dataset
broadband_coverage = broadband_coverage %>%
  # Handle missing values:
  # Replace NA values in numeric columns similarly to the previous dataset
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 
                                            ifelse(sum(is.na(.)) > 0.9 * length(.), 0, median(., na.rm = TRUE)), 
                                            .))) %>%
  # Apply log transformation to numeric columns:
  # Add 1 before applying log transformation
  mutate(across(where(is.numeric), ~ log(. + 1))) 


# ------------------------------------------------------


# Filter datasets for Cornwall postcode areas and merge
cornwall_broadband_data = broadband_performance %>%
  # Filter rows where postcode area is either "TR" or "PL" (Cornwall areas)
  filter(postcode.area %in% c("TR", "PL")) %>%
  # Join the filtered broadband_performance dataset with the filtered broadband_coverage dataset
  inner_join(broadband_coverage %>% filter(pca %in% c("TR", "PL")),
             by = c("postcode", "postcode_space" = "pcds")) %>%
  # Select relevant columns for the final dataset
  select(postcode, postcode_space, postcode.area, 
         Average.download.speed..Mbit.s., Maximum.download.speed..Mbit.s., Minimum.download.speed..Mbit.s.,
         Average.upload.speed..Mbit.s., Maximum.upload.speed..Mbit.s., Minimum.upload.speed..Mbit.s.,
         Average.data.usage..GB., All.Premises, All.Matched.Premises)

# Join the postcode_to_lsoa dataset with the cornwall_broadband_data
cornwall_broadband_data = cornwall_broadband_data %>%
  left_join(postcode_to_lsoa %>%
              select(pcds, lsoa_area = lsoa11nm, county = ladnm), # Select and rename columns
            by = c("postcode_space" = "pcds")) %>% # Join by matching columns
  filter(county == "Cornwall")

# Save the processed and combined data to a CSV file
write.csv(cornwall_broadband_data, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\cornwall_broadband_data.csv", row.names = FALSE)

# Optionally view the dataset in RStudio
View(cornwall_broadband_data)


# ------------------------------------------------------


# Filter datasets for Bristol postcode areas and merge
bristol_broadband_data = broadband_performance %>%
  # Filter rows where postcode area is either "BS" (Bristol area)
  filter(postcode.area %in% c("BS")) %>%
  # Join the filtered broadband_performance dataset with the filtered broadband_coverage dataset
  inner_join(broadband_coverage %>% filter(pca %in% c("BS")),
             by = c("postcode", "postcode_space" = "pcds")) %>%
  # Select relevant columns for the final dataset
  select(postcode, postcode_space, postcode.area, 
         Average.download.speed..Mbit.s., Maximum.download.speed..Mbit.s., Minimum.download.speed..Mbit.s.,
         Average.upload.speed..Mbit.s., Maximum.upload.speed..Mbit.s., Minimum.upload.speed..Mbit.s.,
         Average.data.usage..GB., All.Premises, All.Matched.Premises)

# Join the postcode_to_lsoa dataset with the bristol_broadband_data
bristol_broadband_data = bristol_broadband_data %>%
  left_join(postcode_to_lsoa %>%
              select(pcds, lsoa_area = lsoa11nm, county = ladnm), # Select and rename columns
            by = c("postcode_space" = "pcds")) %>% # Join by matching columns
  filter(county == "Bristol, City of")  # Filter by local authority

# Save the processed and combined data to a CSV file
write.csv(bristol_broadband_data, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\bristol_broadband_data.csv", row.names = FALSE)

# Optionally view the dataset in RStudio
View(bristol_broadband_data)


# ------------------------------------------------------

# Filter datasets for Bristol and Cornwall postcode areas and local authorities, then merge
combined_broadband_data = bind_rows(cornwall_broadband_data, bristol_broadband_data)

# Save the combined data to a CSV file
write.csv(combined_broadband_data, "C:\\Users\\ghimi\\Desktop\\Town-Recommendation\\Clean Data\\Cleaned Broadband Data\\combined_broadband_data.csv", row.names = FALSE)

# Optionally view the dataset in RStudio
View(combined_broadband_data)

