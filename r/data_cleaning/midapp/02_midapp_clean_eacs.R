# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
midapp_interim_data_loc <-
  here("data", "interim", "midapp", "step-01")

# Read in dataset
eacs_data <-
  read_rds(paste0(midapp_interim_data_loc, "/midapp-eacs-data", ".Rds")) %>%
  # remove_empty: removes empty rows and columns
  # remove_constant: removes constant columns
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Reverse score items
# Method: take the maximum value for the scale, add 1, and subtract the participant's score.

# eACS:
# To use the eACS as a one factor measure, first reverse the following items: 1, 2, 3, 4, 5, 7, 8, 13, 14

eacs_data <- eacs_data %>%
  mutate(across(
    .cols = c(
      eacs_midapp_1,
      eacs_midapp_2,
      eacs_midapp_3,
      eacs_midapp_4,
      eacs_midapp_5,
      eacs_midapp_7,
      eacs_midapp_8,
      eacs_midapp_13,
      eacs_midapp_14
    ),
    
    ~ 4 + 1 - .
  ))

# Generate derived variables
eacs_data <- eacs_data %>%
  mutate(# eACS: Sum of all items
    eacs_midapp_total = rowSums(select(., contains("eacs"))))

# Save data
saveRDS(eacs_data,
        here("data", "interim", "midapp", "step-02", "eacs.Rds"))