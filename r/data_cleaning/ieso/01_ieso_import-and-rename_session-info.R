# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in ieso csv file stored in the ieso folder
session_data_raw <-
  read_csv(
    here("data", "raw", "ieso", "session_info.csv"),
    col_names = TRUE,
    trim_ws = TRUE
  )

# Rename ID column to match other datasets
session_data <- session_data_raw %>%
  rename(participant_id = port_id)

# Clean names
session_data <- session_data %>%
  # clean_names: makes all names unique, all lower case & only consist of _, 
  # numbers, and letters
  clean_names()

# Add 'ieso' prefix
`ieso-session-data` <- session_data %>% 
  rename_with(
    .fn = ~paste0("ieso_", .x), 
    .cols = -c(row_number, participant_id)
  )

# Remove datasets that aren't going to be saved
rm(session_data_raw)
rm(session_data)

# Save all dataframe objects in the global environment
save_all_dataframes(path = here("data", "interim", "ieso", "step-01"))