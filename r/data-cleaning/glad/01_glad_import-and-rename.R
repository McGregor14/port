# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in redcap csv file stored in the redcap folder
glad_raw <-
  read_csv(
    here(
      "data",
      "raw",
      "glad",
      "PORT_ids_with_sex_25.01.2023.csv"
    ),
    col_names = TRUE,
    trim_ws = TRUE
  )

# Remove test participants (without PORT_R_ prefix)
glad_data <- glad_raw %>%
  filter(str_detect(port_id, 'PORT_R_'))

# Drop ID prefix (PORT_R_) and rename ID column to match other datasets
glad_data <- glad_data %>%
  mutate(port_id = substring(port_id, 8)) %>%
  rename(participant_id = port_id)

# Clean names
glad_data <- glad_data %>%
  rename(demographics_biological_sex = biological_sex)

# Save data
saveRDS(glad_data,
        here("data", "interim", "glad", "step-01", "glad-data.Rds"))