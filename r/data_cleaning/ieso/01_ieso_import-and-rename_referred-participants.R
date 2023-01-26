# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in redcap csv file stored in the redcap folder
referral_raw <-
  read_csv(
    here(
      "data",
      "raw",
      "ieso",
      "PORTstudy-TotalreferredToIeso_DATA_2023-01-25_2137.csv"
    ),
    col_names = TRUE,
    trim_ws = TRUE
  )

# Remove test participants (without PORT_R_ prefix)
referral_data <- referral_raw %>%
  filter(str_detect(port_id, 'PORT_R_'))

# Drop ID prefix (PORT_R_) and rename ID column to match other datasets
referral_data <- referral_data %>%
  mutate(port_id = substring(port_id, 8)) %>%
  select(participant_id = port_id)

# Save data
saveRDS(referral_data,
        here("data", "interim", "ieso", "step-01", "ieso-referral-data.Rds"))