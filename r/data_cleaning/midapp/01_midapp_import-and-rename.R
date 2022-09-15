# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing

# Source function script
source(file = here("r", "functions.R"))

# Read in midapp csv file stored in the midapp folder
mid_app_raw <-
  read_csv(
    here("data", "raw", "midapp", "2022-07-05_midapp-survey.csv"),
    col_select = 18:80,
    # First few columns contain redundant meta-data
    col_names = TRUE,
    trim_ws = TRUE
  ) %>%
  slice(-c(1, 2)) # Remove second and third rows which contain metadata about the questions from Qualtrics

# Remove test participants (without PORT_R_ prefix)
mid_app_data <- mid_app_raw %>%
  filter(str_detect(port_id, 'PORT_R_'))

# Drop ID prefix (PORT_R_) and rename ID column to match other datasets
mid_app_data <- mid_app_data %>%
  mutate(port_id = substring(port_id, 8)) %>%
  rename(participant_id = port_id)

# Swap numeric indicator for collection wave with 'midapp'
mid_app_data <- mid_app_data %>%
  rename_with(.fn =
                ~ str_replace(., "3$", "midapp"),
              .cols =
                starts_with(c(
                  'eacs',
                  'ius12',
                  'bis-bas',
                  'epq-r-nscale'
                ))) %>%
  
  # Move wave indicator to the middle of the variable name
  rename_with(
    .fn = ~ gsub(pattern = '(eacs|ius12|bis-bas|epq-r-nscale)_(\\d+)_(.*)',
                 replacement = '\\1_\\3_\\2',
                 .x),
    .cols = matches('^(eacs|ius12|bis-bas|epq-r-nscale)')
  ) %>%
  rename_with(
    .fn = ~str_replace_all(., "-", "_"),
    .cols = everything()) %>% 
  select(participant_id, everything())

# Adjust column types
mid_app_data <- mid_app_data %>% 
  mutate(across(.cols = -participant_id, ~as.integer(.))) # All columns except participant ID

# Generate different datasets for each measure

# eACS
`midapp-eacs-data` <- mid_app_data %>%
  select(participant_id, starts_with("eacs"))

# IUS-12
`midapp-ius-data` <- mid_app_data %>%
  select(participant_id, starts_with("ius12"))

# BIS-BAS
`midapp-bis_bas-data` <- mid_app_data %>%
  select(participant_id, starts_with("bis_bas"))

# EPQ-R-N
`midapp-epq_r_n-data` <- mid_app_data %>%
  select(participant_id, starts_with("epq_r_n"))

# Remove datasets that aren't going to be saved
rm(mid_app_raw)
rm(mid_app_data)

# Save all dataframe objects in the global environment
save_all_dataframes(path = here("data", "interim", "midapp", "step-01"))