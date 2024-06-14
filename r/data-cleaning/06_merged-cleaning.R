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

# Create derived variables ------------------------------------------------

# Create new variables from merged dataset variables

# Referrals
# Missing referral data only exists for participants that weren't referred.
# Change all missing values to FALSE for referrals.
port_data <-
  port_data %>%
  mutate(ieso_referral = if_else(is.na(ieso_referral),
                                 FALSE,
                                 ieso_referral))

port_data %>% 
  count(ieso_referral)

# Exclusions

# Check number of participants excluded for their FLARe/Ieso data (filter out
# participants that didn't complete flare or weren't referred to ieso if you 
# don't want to see missing values)
port_data %>%
  count(flare_exclusion_port, ieso_exclusion_port)

# Check number of participants excluded for their FLARe/Ieso data
# this time checking if they passed screening
port_data %>%
  count(port_screening_eligibility, flare_exclusion_port, ieso_exclusion_port)

# Check IDs for participants that weren't exclusions but were not eligible after
# screening
port_data %>% 
  filter(port_screening_eligibility == FALSE) %>% 
  pull(participant_id)

# Create a port exclusion variable that identifies anyone excluded based on
# their FLARe or Ieso data
port_data <-
  port_data %>%
  mutate(
    port_exclusion = case_when(
      flare_exclusion_port == FALSE &
        ieso_exclusion_port == FALSE &
        port_screening_eligibility == TRUE
      ~ FALSE,
      TRUE ~ TRUE
    ),
    .after = participant_id
  )

# Check number of port exclusions
port_data %>%
  count(port_exclusion)

# Dates

# Followup time since therapy
# Time between last therapy session and the followup survey
# Note: ieso_end_date variable not used as discharge can happen quite some time
# after the last treatment session and in one case was after a followup survey
port_data <-
  port_data %>%
  mutate(
    treatment_followup_time_since_therapy_days =
      ieso_last_treatment_date %--% followup_date / days(1)
  )

# Save data
saveRDS(port_data,
        here("data", "processed", "merged", "06_clean-processed-data.Rds"))
