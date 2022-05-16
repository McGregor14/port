# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(lubridate) # date/time manipulations
library(fs) # file system operations

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder raw data is stored in
raw_data_loc <- here("data", "raw")

# Find the name of the latest folder exported from the flare website
flare_data_loc <- find_latest_flare_folder(experiment_name = "PORT", path_name = raw_data_loc)

# Create a pathname from the location of the raw data and the latest flare folder export
flare_data_path <- paste0(raw_data_loc, "/", flare_data_loc)

# List all the csv files stored in the latest flare folders
flare_csv_files <- dir_ls(flare_data_path, regexp = "\\.csv$")

# Read in all csv files stored in the latest flare folder
flare_dat_list <- flare_csv_files %>% 
  map(~read_csv(., col_names = TRUE, trim_ws = TRUE))

# Remove the pathname, duplicate prefix, and filetype from the names of the list elements
flare_dat_names <- flare_dat_list %>%
  names() %>% 
  str_remove_all(., paste0(flare_data_path, "/", flare_data_loc, "-")) %>%
  str_remove(., ".csv") %>% 
  str_replace_all(., "-", "_")

# Set the new names of the list elements
names(flare_dat_list) <- flare_dat_names

# Remove all elements of the list with 0 rows
flare_dat_list <- keep(flare_dat_list, ~ nrow(.x) > 0)

# Remove the 'completed participants' dataframe
flare_dat_list <-  flare_dat_list[names(flare_dat_list) != "completed_participant_ids"]

# Print a message giving the names of modules with no data
empty_modules <- flare_dat_names[which(flare_dat_names %!in% names(flare_dat_list))]
message(paste0("Modules with no data: ", toString(empty_modules)))

# Remove test participants (without PORT_R_ prefix) and drop ID prefix (PORT_R_)
flare_dat_list <- map2(flare_dat_list, names(flare_dat_list), ~ filter(.x, str_detect(participant_id, 'PORT_R_')))

flare_dat_list <- flare_dat_list %>% 
  map(modify_at, "participant_id", substring, 8)

# Drop redundant columns shared across all dataframes in the list
flare_dat_list <- map(flare_dat_list, ~ (.x %>% select(-c('experiment_id', 'experiment_code'))))

# Split all elements in the list into separate objects in the environment
flare_dat_list %>% 
  list2env(., envir = .GlobalEnv)

# Remove original grouped list of datasets from environment
rm(flare_dat_list)

# Save all dataframe objects in the global environment
save_all_dataframes(path = here("data", "interim","flare"))