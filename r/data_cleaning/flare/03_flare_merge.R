# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(fs) # file system operations

# Specify the location of data files from step 02 of cleaning
flare_data_path <- (here("data", "interim", "flare", "step-02"))

# List all the RDS files stored in the latest flare folders
flare_rds_files <- dir_ls(flare_data_path, regexp = "\\.Rds$")

# Read in all RDS files stored in the latest flare folder
flare_data_list <- flare_rds_files %>% 
  map(~read_rds(.))

# Remove the pathname, duplicate prefix, and filetype from the names of the list elements
flare_data_names <- flare_data_list %>%
  names() %>% 
  str_remove_all(., paste0(flare_data_path, "/")) %>%
  str_remove(., ".Rds")

# Set the new names of the list elements
names(flare_data_list) <- flare_data_names

# Reorder the list ready for merging (doesn't seem to be making a difference)
flare_data_list <- flare_data_list[c("participants",
                                     "basic-info", 
                                     "affective-ratings", 
                                     "fear-conditioning", 
                                     "contingency-awareness", 
                                     "us-unpleasantness", 
                                     "post-experiment-questions")]

# Merge all elements of the list
flare_data <- flare_data_list %>% 
  reduce(full_join, by = "participant_id")

# Save data
saveRDS(flare_data, here("data", "processed", "flare-data.Rds"))
