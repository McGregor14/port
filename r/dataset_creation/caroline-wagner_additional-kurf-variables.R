# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(fs) # file system operations

# Read in dataset
port <-
  read_rds(here("data", "processed", "merged", "06_clean-processed-data.Rds"))

# Read in IDs
kurf_ids <-
  read_rds(here(
    "data",
    "data-sharing",
    "ingoing",
    "kurf-participant-id-data.rds"
  ))

# Filter port data for observations in KURF data
port <-
  port %>%
  filter(participant_id %in% kurf_ids$participant_id)

# Select relevant variables
port <-
  port %>%
  select(
    participant_id,
    contains("date"),
    treatment_followup_time_since_therapy_days,
    contains("demographics")
  )

# Write data
write_rds(
  port,
  here(
    "data",
    "data-sharing",
    "outgoing",
    "caroline-wagner_additional-kurf-variables.rds"
  )
)
