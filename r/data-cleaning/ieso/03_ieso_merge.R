# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(fs) # file system operations

# Specify the location of data files from step 02 of cleaning
ieso_data_path <- (here("data", "interim", "ieso", "step-02"))

# List all the RDS files stored in the folder
ieso_rds_files <- dir_ls(ieso_data_path, regexp = "\\.Rds$")

# Read in all RDS files stored in the folder
ieso_data_list <- ieso_rds_files %>%
  map( ~ read_rds(.))

# Remove the pathname, duplicate prefix, and filetype from the names of the 
# list elements
ieso_data_names <- ieso_data_list %>%
  names() %>%
  str_remove_all(., paste0(ieso_data_path, "/")) %>%
  str_remove(., ".Rds")

# Set the new names of the list elements
names(ieso_data_list) <- ieso_data_names

# Reorder the list ready for merging
ieso_data_list <- ieso_data_list[c("patient-info", "session-info")]

# Merge all elements of the list
ieso_data <- ieso_data_list %>%
  reduce(full_join, by = "participant_id")

# Save data
saveRDS(ieso_data, here("data", "interim", "ieso", "step-03", "ieso-data.Rds"))
