# NOTE: This dataset was added quite a while after the others. As such, it is
# a bit out of sync (i.e. reading in from raw at step 2 rather than step 1).

# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that raw data is stored in
redcap_raw_data_loc <-
  here("data", "raw", "redcap")

# Read in dataset
online_eligible_data <-
  read_csv(
    paste0(
      redcap_raw_data_loc,
      "/2023-07-18_online-screened-and-eligible_dataset_tm",
      ".csv"
    ),
    col_names = TRUE
  ) %>%
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Remove test participants (without PORT_R_ prefix)
online_eligible_data <- online_eligible_data %>%
  filter(str_detect(port_id, 'PORT_R_'))

# Drop ID prefix (PORT_R_) and rename ID column to match other datasets
online_eligible_data <- online_eligible_data %>%
  mutate(port_id = substring(port_id, 8)) %>%
  rename(participant_id = port_id)

# Generate derived variables
online_eligible_data <- online_eligible_data %>%
  mutate(completed_online_screening = TRUE)

# Save data
saveRDS(online_eligible_data,
        here("data", "interim", "redcap", "step-02", "online_eligible.Rds"))
