# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(lubridate) # date/time manipulations
library(fs) # file system operations

# Source function scripts
source(file = )

# Specify the subfolder raw data is stored in
raw_data_loc <- 

# Find the name of the latest folder exported from the flare website
flare_data_loc <- find_latest_flare_file(experiment_name = "PORT", path_name = raw_data_loc)

# Create a pathname from the location of the raw data and the latest flare folder export
flare_data_path <- paste0(raw_data_loc, flare_data_loc)

# List all the csv files stored in the latest flare folders
flare_csv_files <- fs::dir_ls(flare_data_path, regexp = "\\.csv$")

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

# Split all elements in the list into separate objects in the environment
flare_dat_list %>% 
  list2env(., envir = .GlobalEnv)
