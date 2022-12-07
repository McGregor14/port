# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
flare_interim_data_loc <-
  here("data", "interim", "flare", "step-01")

# Read in dataset
contingency_awareness_raw <-
  read_rds(paste0(
    flare_interim_data_loc,
    "/contingency-awareness-data",
    ".Rds"
  )) %>%
  # clean_names: makes all names unique, all lower case & only consist of _,
  # numbers, and letters
  clean_names() %>%
  # remove_empty: removes empty rows and columns
  remove_empty(which = c("rows", "cols")) %>%
  # remove_constant: removes constant columns
  remove_constant(na.rm = T, quiet = F)

# Clean dataset
contingency_awareness <- contingency_awareness_raw %>%
  rename(
    contingency_awareness_answer = awareness_answer,
    contingency_awareness_stimulus_guess = confirmation_answer,
    contingency_aware = is_aware
  ) %>%
  rename_with(.fn = ~ paste0("flare_", .x),
              .cols = -participant_id)


# Save data
saveRDS(
  contingency_awareness,
  here(
    "data",
    "interim",
    "flare",
    "step-02",
    "contingency-awareness.Rds"
  )
)

