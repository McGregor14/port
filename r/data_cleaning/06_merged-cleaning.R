# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(zoo) # lead and lag functionality (na.locf0)

# Source function script
source(file = here("r", "functions.R"))

# Read in dataset
port_data <-
  read_rds(here("data", "processed", "merged", "05_merged-processed-data.Rds"))


# Clean existing variables ------------------------------------------------

# Replace missing values with FALSE for eligibility variables
port_data <- port_data %>%
  mutate(across(
    .cols =
      c(completed_online_screening, completed_phone_screening),
    .fns = ~ replace_na(., FALSE)
  ))

# Create derived variables ------------------------------------------------

# Create new variables from merged dataset variables

# Exclusions

# Check number of participants excluded for their FLARe/Ieso data
port_data %>%
  count(flare_exclusion_port, ieso_exclusion_port)

# Create a port exclusion variable that identifies anyone excluded based on
# their FLARe or Ieso data
port_data <-
  port_data %>%
  mutate(
    port_exclusion = case_when(
      completed_online_screening == TRUE &
        completed_phone_screening == TRUE &
        flare_exclusion_port == FALSE &
        ieso_exclusion_port == FALSE
      ~ FALSE,
      TRUE ~ TRUE
    ),
    .after = participant_id
  )

# Check number of port exclusions
port_data %>%
  count(port_exclusion)

# Save data
saveRDS(port_data,
        here("data", "processed", "merged", "06_clean-processed-data.Rds"))
