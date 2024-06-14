# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(fs) # file system operations

# Specify the location of data files from step 02 of cleaning
midapp_data_path <- (here("data", "interim", "midapp", "step-02"))

# List all the RDS files stored in the folder
midapp_rds_files <- dir_ls(midapp_data_path, regexp = "\\.Rds$")

# Read in all RDS files stored in the folder
midapp_data_list <- midapp_rds_files %>%
  map( ~ read_rds(.))

# Remove the pathname, duplicate prefix, and filetype from the names of the list elements
midapp_data_names <- midapp_data_list %>%
  names() %>%
  str_remove_all(., paste0(midapp_data_path, "/")) %>%
  str_remove(., ".Rds")

# Set the new names of the list elements
names(midapp_data_list) <- midapp_data_names

# Reorder not required, already in alphabetical

# Merge all elements of the list
midapp_data <- midapp_data_list %>%
  reduce(full_join, by = "participant_id")

# Save data
saveRDS(midapp_data, here("data", "processed", "midapp-data.Rds"))
