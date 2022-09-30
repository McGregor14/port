# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in ieso csv file stored in the ieso folder
patient_data_raw <-
  read_csv(
    here("data", "raw", "ieso", "patient_info.csv"),
    col_names = TRUE,
    trim_ws = TRUE
  )

# Rename ID column to match other datasets
patient_data <- patient_data_raw %>%
  rename(participant_id = port_id)

# Clean names
patient_data <- patient_data %>%
  clean_names()

# Add 'ieso' prefix
`ieso-patient-data` <- patient_data %>% 
  rename_with(
    .fn = ~paste0("ieso_", .x), 
    .cols = -participant_id
  )

# Remove datasets that aren't going to be saved
rm(patient_data_raw)
rm(patient_data)

# Save all dataframe objects in the global environment
save_all_dataframes(path = here("data", "interim", "ieso", "step-01"))