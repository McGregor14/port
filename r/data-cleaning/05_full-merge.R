# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(fs) # file system operations

# Specify the location of data files from step 02 of cleaning
processed_data_path <- (here("data", "processed"))

# List all the RDS files stored in the folder
processed_rds_files <-
  dir_ls(processed_data_path, regexp = "\\.Rds$")

# Read in all RDS files stored in the folder
processed_data_list <- processed_rds_files %>%
  map(~ read_rds(.))

# Remove the pathname, duplicate prefix, and filetype from the names of the
# list elements
processed_data_names <- processed_data_list %>%
  names() %>%
  str_remove_all(., paste0(processed_data_path, "/")) %>%
  str_remove(., ".Rds")

# Set the new names of the list elements
names(processed_data_list) <- processed_data_names

# Remove long datasets from merge
processed_data_list <-
  processed_data_list %>%
  
  # Set treatment_data_long to null
  purrr::list_modify("treatment_data_long" = NULL) %>%
  
  # Drop list elements that are empty
  compact()

# Reorder the list ready for merging
processed_data_list <-
  processed_data_list[c("glad-data",
                        "redcap-data",
                        "midapp-data",
                        "flare-data",
                        "ieso-data")]

# Merge all elements of the list
processed_data <-
  processed_data_list %>%
  reduce(full_join, by = "participant_id")

# Reorder biological sex variable
processed_data <-
  processed_data %>%
  relocate(demographics_biological_sex,
           .after = demographics_age_at_screening_years)

# Read in the IDs from Redcap. All IDs in this dataset are for consented
# participants
port_ids <-
  read_rds(here("data", "processed", "redcap-data.Rds")) %>%
  pull(participant_id)

# Filter out any participants that weren't in the Redcap dataset, i.e. they 
# didn't consent (their data won't have been in merged but their empty row from
# flare may have)
processed_data <-
  processed_data %>%
  filter(participant_id %in% port_ids)

processed_data %>% 
  count(ieso_exclusion_port)

# Save data
saveRDS(
  processed_data,
  here("data", "processed", "merged", "05_merged-processed-data.Rds")
)
